import json
from pathlib import Path

import pytest
from aggregate_rq2_rq3 import summarize_invalid, summarize_valid
from rq_records import exception_records


def test_missing_valid_data_is_not_zero(tmp_path: Path) -> None:
    with pytest.raises((FileNotFoundError, ValueError)):
        summarize_valid(str(tmp_path / "missing"))


def test_invalid_limit_preserves_lexicographic_path_order(tmp_path: Path) -> None:
    folder = tmp_path / "exception" / "unexpected"
    folder.mkdir(parents=True)
    for program_id, message in [(1, "A"), (2, "A"), (10, "B")]:
        (folder / f"{program_id}.json").write_text(
            json.dumps(
                {
                    "programId": program_id,
                    "results": [{"library": "OpenFHE", "failedResult": message}],
                }
            )
        )
    items = exception_records([str(tmp_path)], 2)
    assert [path.name for path, _ in items] == ["1.json", "10.json"]
    stats = summarize_invalid([str(tmp_path)], 2)
    assert stats["unique"] == 2
    assert stats["unexpected"] == 2
    assert stats["expected"] == 0


def test_invalid_limit_sorts_paths_across_chunks(tmp_path: Path) -> None:
    directories = [tmp_path / "iter2", tmp_path / "iter10"]
    for directory in directories:
        folder = directory / "exception" / "unexpected"
        folder.mkdir(parents=True)
        (folder / "0.json").write_text(json.dumps({"programId": 0}))
    items = exception_records([str(path) for path in directories], 1)
    assert items[0][0].is_relative_to(directories[1])


def test_corrupt_json_is_rejected(tmp_path: Path) -> None:
    for category in ("succ", "fail", "psr_err"):
        (tmp_path / category).mkdir()
    (tmp_path / "succ" / "0.json").write_text("{")
    with pytest.raises(ValueError):
        summarize_valid(str(tmp_path))


def make_manifest(root: Path, repeats: int = 3) -> Path:
    lines = [f"BASELINE_REPEATS={repeats}", "MODE=smoke", "RUN_FINISHED=1"]
    for enc in ("int", "double"):
        for repeat in range(repeats + 1):
            valid = root / f"valid-{enc}-{repeat}"
            for category in ("succ", "fail", "psr_err"):
                (valid / category).mkdir(parents=True)
            for program_id in range(3):
                category = "succ" if program_id < repeat else "fail"
                (valid / category / f"{program_id}.json").write_text(
                    json.dumps({"programId": program_id})
                )
            key = f"random_dir_{enc}_{repeat}" if repeat else f"valid_dir_{enc}"
            lines.append(f"{key}={valid}")
            invalid = root / f"invalid-{enc}-{repeat}"
            folder = invalid / "exception" / "expected"
            folder.mkdir(parents=True)
            for program_id in range(3):
                (folder / f"{program_id}.json").write_text(
                    json.dumps(
                        {
                            "programId": program_id,
                            "results": [
                                {"library": "OpenFHE", "failedResult": str(program_id)}
                            ],
                        }
                    )
                )
            key = (
                f"invalid_random_dirs_{enc}_{repeat}"
                if repeat
                else f"invalid_dir_{enc}"
            )
            lines.append(f"{key}={invalid}")
    manifest = root / "run.txt"
    manifest.write_text("\n".join(lines))
    return manifest


def test_three_repeats_mean_and_sample_sd(tmp_path: Path) -> None:
    import csv

    from aggregate_rq2_rq3 import aggregate

    manifest = make_manifest(tmp_path)
    output = tmp_path / "out"
    aggregate(manifest, output)
    with (output / "baseline_statistics.csv").open() as stream:
        row = next(csv.DictReader(stream))
    assert int(row["n"]) == 3
    assert float(row["mean"]) == pytest.approx(200 / 3)
    assert float(row["sample_sd"]) == pytest.approx(100 / 3)


def test_partial_repeats_do_not_produce_statistics(tmp_path: Path) -> None:
    import csv

    from aggregate_rq2_rq3 import InputError, aggregate

    manifest = make_manifest(tmp_path)
    (tmp_path / "valid-int-2" / "succ" / "0.json").unlink()
    output = tmp_path / "out"
    with pytest.raises(InputError, match="Incomplete"):
        aggregate(manifest, output)
    assert not output.exists()
    aggregate(manifest, output, allow_partial=True)
    with (output / "baseline_statistics.csv").open() as stream:
        rows = list(csv.DictReader(stream))
    assert not any(
        row["encType"] == "int" and row["metric"] == "success_rate_percent"
        for row in rows
    )


def test_unfinished_manifest_is_rejected(tmp_path: Path) -> None:
    from aggregate_rq2_rq3 import InputError, aggregate

    manifest = make_manifest(tmp_path)
    manifest.write_text(
        manifest.read_text().replace("RUN_FINISHED=1", "RUN_FINISHED=0")
    )
    with pytest.raises(InputError, match="did not finish"):
        aggregate(manifest, tmp_path / "out", allow_partial=True)


def test_old_manifest_does_not_hide_an_earlier_failure(tmp_path: Path) -> None:
    from aggregate_rq2_rq3 import InputError, parse_summary_file

    manifest = tmp_path / "old.txt"
    manifest.write_text("exit=1\nexit=0\n")
    with pytest.raises(InputError, match="Abnormal"):
        parse_summary_file(manifest)


def test_cli_exports_and_rejects_missing_data(tmp_path: Path) -> None:
    import subprocess
    import sys

    manifest = make_manifest(tmp_path)
    script = Path(__file__).resolve().parents[1] / "aggregate_rq2_rq3.py"
    command = [sys.executable, str(script), "--summary", str(manifest)]
    completed = subprocess.run(command, capture_output=True, text=True, check=False)
    assert completed.returncode == 0, completed.stderr
    assert (tmp_path / "aggregated" / "baseline_statistics.csv").is_file()
    (tmp_path / "valid-int-1" / "psr_err").rmdir()
    failed = subprocess.run(command, capture_output=True, text=True, check=False)
    assert failed.returncode == 1
    assert "Aggregation failed" in failed.stderr


def test_generated_count_matching_preserves_zero_record_runs(tmp_path: Path) -> None:
    from aggregate_rq2_rq3 import InputError, aggregate

    manifest = make_manifest(tmp_path, repeats=1)
    lines = [manifest.read_text(), "VALID_COUNT_BASIS=generated"]
    for enc in ("int", "double"):
        lines.extend(
            [
                f"valid_generated_count_{enc}=4",
                f"random_generated_count_{enc}_1=4",
                f"exit_RQ2-valid-{enc}=124",
                f"exit_RQ2-random-{enc}-repeat1=0",
            ]
        )
    manifest.write_text("\n".join(lines))
    for category in ("succ", "fail", "psr_err"):
        for path in (tmp_path / "valid-double-1" / category).glob("*.json"):
            path.unlink()
    output = tmp_path / "out"
    aggregate(manifest, output)
    summary = json.loads((output / "rq2_rq3_summary.json").read_text())
    row = next(
        row
        for row in summary["valid"]
        if row["encType"] == "double" and row["repeat"] == 1
    )
    assert row["total"] == 0
    assert row["generated"] == row["unrecorded"] == 4
    assert row["succ_rate"] == 0
    assert row["complete"]
    manifest.write_text(
        manifest.read_text().replace(
            "exit_RQ2-random-double-repeat1=0", "exit_RQ2-random-double-repeat1=124"
        )
    )
    with pytest.raises(InputError, match="Incomplete"):
        aggregate(manifest, tmp_path / "timed-out")


def test_missing_generated_count_is_rejected(tmp_path: Path) -> None:
    from aggregate_rq2_rq3 import InputError, aggregate

    manifest = make_manifest(tmp_path)
    manifest.write_text(manifest.read_text() + "\nVALID_COUNT_BASIS=generated\n")
    with pytest.raises(InputError, match="Missing valid_generated_count_int"):
        aggregate(manifest, tmp_path / "out")
