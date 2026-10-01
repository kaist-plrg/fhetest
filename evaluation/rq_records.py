"""Read the result fields used by RQ2/RQ3; reject incomplete input archives."""

from pathlib import Path
import statistics
from typing import ClassVar, TypedDict

from pydantic import BaseModel, ConfigDict, Field


class Failure(BaseModel):
    model_config: ClassVar[ConfigDict] = ConfigDict(frozen=True, strict=True)
    library: str
    failedResult: str


class Record(BaseModel):
    model_config: ClassVar[ConfigDict] = ConfigDict(frozen=True, strict=True)
    programId: int = Field(ge=0)
    failures: list[Failure] = Field(default_factory=list)
    results: list[Failure] = Field(default_factory=list)


class ValidStats(TypedDict):
    succ: int
    fail: int
    psr_err: int
    total: int


class InvalidStats(TypedDict):
    exceptions: int
    unique: int
    expected: int
    unexpected: int


class Row(TypedDict):
    encType: str
    kind: str
    repeat: int
    runDir: str
    target: int
    total: int
    generated: int | None
    unrecorded: int | None
    succ: int
    fail: int
    psr_err: int
    succ_rate: float
    recorded_succ_rate: float
    exceptions: int
    unique_messages: int
    expected: int
    unexpected: int
    complete: bool


class Statistic(TypedDict):
    encType: str
    metric: str
    n: int
    mean: float
    sample_sd: float | None


def baseline_statistics(rows: list[Row]) -> list[Statistic]:
    result: list[Statistic] = []
    for enc in ("int", "double"):
        for kind, metric in (
            ("random", "success_rate_percent"),
            ("invalid_random", "unique_messages"),
        ):
            selected = [
                row for row in rows if row["encType"] == enc and row["kind"] == kind
            ]
            if not selected or not all(row["complete"] for row in selected):
                continue
            values = [
                row["succ_rate"] * 100
                if kind == "random"
                else float(row["unique_messages"])
                for row in selected
            ]
            result.append(
                Statistic(
                    encType=enc,
                    metric=metric,
                    n=len(values),
                    mean=statistics.mean(values),
                    sample_sd=statistics.stdev(values) if len(values) > 1 else None,
                )
            )
    return result


def records(directory: Path) -> list[tuple[Path, Record]]:
    if not directory.is_dir():
        raise FileNotFoundError(directory)
    result = [
        (path, Record.model_validate_json(path.read_bytes()))
        for path in directory.rglob("*.json")
    ]
    return sorted(result, key=lambda item: (item[1].programId, str(item[0])))


def exception_records(
    run_dirs: list[str], limit: int | None = None
) -> list[tuple[Path, Record]]:
    result = [
        item
        for directory in run_dirs
        for item in records(Path(directory) / "exception")
    ]
    result.sort(key=lambda item: str(item[0]))
    return result[:limit] if limit is not None else result


def summarize_valid(run_dir: str) -> ValidStats:
    counts = [
        len(records(Path(run_dir) / category))
        for category in ("succ", "fail", "psr_err")
    ]
    return ValidStats(
        succ=counts[0], fail=counts[1], psr_err=counts[2], total=sum(counts)
    )


def summarize_invalid(run_dirs: list[str], limit: int | None = None) -> InvalidStats:
    items = exception_records(run_dirs, limit)
    messages = {
        failure.failedResult
        for _, record in items
        for failure in record.results
        if failure.library == "OpenFHE" and failure.failedResult
    }
    return InvalidStats(
        exceptions=len(items),
        unique=len(messages),
        expected=sum(path.parent.name == "expected" for path, _ in items),
        unexpected=sum(path.parent.name == "unexpected" for path, _ in items),
    )
