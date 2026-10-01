#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = ["pydantic==2.12.5"]
# ///
# How to run: uv run evaluation/aggregate_rq2_rq3.py --summary RUN.txt
"""Aggregate isolated runs and baseline repetitions without hiding missing data."""

import argparse
import csv
import json
from pathlib import Path
from rq_records import (
    Row,
    Statistic,
    baseline_statistics,
    exception_records,
    records,
    summarize_invalid,
    summarize_valid,
)


class Arguments(argparse.Namespace):
    summary: Path | None = None
    outdir: Path | None = None
    allow_partial: bool = False


class InputError(ValueError):
    """An experiment manifest cannot support the requested aggregation."""


def parse_summary_file(path: Path) -> dict[str, str]:
    values: dict[str, str] = {}
    for line in path.read_text().splitlines():
        if "=" not in line or line.startswith("=="):
            continue
        key, value = line.strip().split("=", 1)
        if (key == "exit" or key.startswith("exit_")) and value not in ("0", "124"):
            raise InputError(f"Abnormal execution: {key}={value}")
        values[key] = value
    return values


def write_rows(path: Path, rows: list[Row]) -> None:
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(Row.__annotations__))
        writer.writeheader()
        writer.writerows(rows)


def aggregate(summary_path: Path, outdir: Path, allow_partial: bool = False) -> None:
    summary = parse_summary_file(summary_path)
    if summary.get("VALID_COUNT_BASIS", "recorded") not in ("recorded", "generated"):
        raise InputError("Unknown VALID_COUNT_BASIS")
    generated_basis = summary.get("VALID_COUNT_BASIS") == "generated"
    repeats = int(summary.get("BASELINE_REPEATS", "1"))
    if repeats < 1:
        raise InputError("BASELINE_REPEATS must be positive")
    indexed = "BASELINE_REPEATS" in summary
    if indexed and summary.get("RUN_FINISHED") != "1":
        raise InputError("Run did not finish; inspect its execution logs")
    rows: list[Row] = []
    legacy: list[tuple[Path, list[str], list[list[str | int]]]] = []
    for enc in ("int", "double"):
        for invalid in (False, True):
            guided_key = f"invalid_dir_{enc}" if invalid else f"valid_dir_{enc}"
            if not summary.get(guided_key):
                raise InputError(f"Missing {guided_key}")
            guided_dir = summary[guided_key]
            if (
                generated_basis
                and not invalid
                and f"valid_generated_count_{enc}" not in summary
            ):
                raise InputError(f"Missing valid_generated_count_{enc}")
            target = (
                summarize_invalid([guided_dir])["exceptions"]
                if invalid
                else int(summary[f"valid_generated_count_{enc}"])
                if generated_basis
                else summarize_valid(guided_dir)["total"]
            )
            for repeat in range(repeats + 1):
                suffix = f"_{repeat}" if indexed else ""
                key = (
                    f"invalid_random_dirs_{enc}{suffix}"
                    if invalid
                    else f"random_dir_{enc}{suffix}"
                )
                directories = (
                    [guided_dir] if repeat == 0 else summary.get(key, "").split(",")
                )
                if not all(directories):
                    raise InputError(f"Missing {key}")
                row = Row(
                    encType=enc,
                    kind=("invalid" if invalid else "valid")
                    if repeat == 0
                    else ("invalid_random" if invalid else "random"),
                    repeat=repeat,
                    runDir=",".join(directories),
                    target=target,
                    total=0,
                    generated=None,
                    unrecorded=None,
                    succ=0,
                    fail=0,
                    psr_err=0,
                    succ_rate=0.0,
                    recorded_succ_rate=0.0,
                    exceptions=0,
                    unique_messages=0,
                    expected=0,
                    unexpected=0,
                    complete=False,
                )
                details: list[list[str | int]] = []
                if invalid:
                    items = exception_records(directories, target if repeat else None)
                    stats = summarize_invalid(directories, target if repeat else None)
                    row.update(
                        exceptions=stats["exceptions"],
                        unique_messages=stats["unique"],
                        expected=stats["expected"],
                        unexpected=stats["unexpected"],
                        complete=target > 0 and stats["exceptions"] == target,
                    )
                    for _, record in items:
                        messages = {
                            entry.library: entry.failedResult
                            for entry in record.results
                        }
                        details.append(
                            [
                                record.programId,
                                messages.get("SEAL", ""),
                                messages.get("OpenFHE", ""),
                            ]
                        )
                else:
                    valid = summarize_valid(directories[0])
                    generated = None
                    finished = True
                    if generated_basis:
                        count_key = (
                            f"random_generated_count_{enc}_{repeat}"
                            if repeat
                            else f"valid_generated_count_{enc}"
                        )
                        if count_key not in summary:
                            raise InputError(f"Missing {count_key}")
                        generated = int(summary[count_key])
                        if generated < valid["total"] or generated < 0:
                            raise InputError(
                                f"Inconsistent generated/result counts: {count_key}"
                            )
                        label = (
                            f"RQ2-random-{enc}-repeat{repeat}"
                            if repeat
                            else f"RQ2-valid-{enc}"
                        )
                        finished = summary.get(f"exit_{label}") == "0" or repeat == 0
                    denominator = generated if generated is not None else valid["total"]
                    row.update(
                        total=valid["total"],
                        generated=generated,
                        unrecorded=generated - valid["total"]
                        if generated is not None
                        else None,
                        succ=valid["succ"],
                        fail=valid["fail"],
                        psr_err=valid["psr_err"],
                        succ_rate=valid["succ"] / denominator if denominator else 0.0,
                        recorded_succ_rate=valid["succ"] / valid["total"]
                        if valid["total"]
                        else 0.0,
                        complete=target > 0 and denominator == target and finished,
                    )
                    for _, record in records(Path(directories[0]) / "fail"):
                        messages = {
                            entry.library: entry.failedResult
                            for entry in record.failures
                        }
                        details.append([record.programId, messages.get("OpenFHE", "")])
                if not row["complete"] and not allow_partial:
                    raise InputError(
                        f"Incomplete {row['kind']} {enc} repeat {repeat}; target={target}"
                    )
                rows.append(row)
                part = "b" if invalid else "a"
                filtered = "On" if repeat == 0 else "Off"
                repeat_suffix = f"-repeat{repeat}" if indexed and repeat else ""
                filename = (
                    f"output-rq2-1-{part}-filter{filtered}-{enc}{repeat_suffix}.csv"
                )
                headers = (
                    ["programId", "SEAL", "OpenFHE"]
                    if invalid
                    else ["programId", "OpenFHE"]
                )
                legacy.append((outdir / filename, headers, details))
    statistics_rows = baseline_statistics(rows)
    # All inputs have been checked before any result files are written.
    outdir.mkdir(parents=True, exist_ok=True)
    write_rows(
        outdir / "rq2_valid_summary.csv",
        [row for row in rows if "invalid" not in row["kind"]],
    )
    write_rows(
        outdir / "rq2_rq3_invalid_summary.csv",
        [row for row in rows if "invalid" in row["kind"]],
    )
    with (outdir / "baseline_statistics.csv").open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(Statistic.__annotations__))
        writer.writeheader()
        writer.writerows(statistics_rows)
    _ = (outdir / "rq2_rq3_summary.json").write_text(
        json.dumps(
            {
                "summary_file": str(summary_path),
                "mode": summary.get("MODE", "unknown"),
                "valid_count_basis": summary.get("VALID_COUNT_BASIS", "recorded"),
                "valid": [row for row in rows if "invalid" not in row["kind"]],
                "invalid": [row for row in rows if "invalid" in row["kind"]],
                "baseline_statistics": statistics_rows,
            },
            indent=2,
        )
        + "\n"
    )
    for path, headers, details in legacy:
        with path.open("w", newline="") as stream:
            writer = csv.writer(stream)
            writer.writerow(headers)
            writer.writerows(details)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    _ = parser.add_argument("--summary", type=Path)
    _ = parser.add_argument("--outdir", type=Path)
    _ = parser.add_argument(
        "--allow-partial",
        action="store_true",
        help="Export partial smoke data; omit incomplete baseline statistics",
    )
    args = parser.parse_args(namespace=Arguments())
    root = Path(__file__).resolve().parent
    candidates = list(root.glob("rq2_rq3_run_*.txt")) + list(
        root.glob("evaluation-*/rq2_rq3_run_*.txt")
    )
    summary = args.summary or (
        max(candidates, key=lambda path: path.stat().st_mtime) if candidates else None
    )
    if summary is None:
        parser.error("No run manifest found; provide --summary")
    outdir = args.outdir or summary.parent / "aggregated"
    try:
        aggregate(summary, outdir, args.allow_partial)
    except (OSError, ValueError) as error:
        parser.exit(1, f"Aggregation failed: {error}\n")
    print(f"Wrote aggregates to {outdir}")


if __name__ == "__main__":
    main()
