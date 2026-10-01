"""Exercise runner bookkeeping using a controlled executable, not HE results."""

import shutil
import subprocess
import json
from pathlib import Path

import pytest
from aggregate_rq2_rq3 import aggregate


@pytest.mark.parametrize("mode", ["smoke", "full"])
def test_runner_isolates_repeats_and_preserves_failure(
    tmp_path: Path, mode: str
) -> None:
    root = tmp_path / "checkout"
    (root / "evaluation").mkdir(parents=True)
    (root / "bin").mkdir()
    subprocess.run(["git", "init", str(root)], check=True, capture_output=True)
    subprocess.run(
        [
            "git",
            "-C",
            str(root),
            "-c",
            "user.name=Test",
            "-c",
            "user.email=test@example.invalid",
            "commit",
            "--allow-empty",
            "-m",
            "fixture",
        ],
        check=True,
        capture_output=True,
    )
    source = Path(__file__).resolve().parents[1] / "run_rq2_rq3.sh"
    runner = root / "evaluation" / source.name
    shutil.copyfile(source, runner)
    executable = root / "bin" / "fhetest"
    executable.write_text("""#!/usr/bin/env bash
set -eu
if [[ "${FAIL_RUN:-0}" == 1 ]]; then echo deliberate-failure; exit 7; fi
case " $* " in
  *" -filter:false "*)
    mkdir -p "$FHETEST_RUN_DIR/exception/expected"
    echo '{"programId":0,"results":[]}' > "$FHETEST_RUN_DIR/exception/expected/0.json" ;;
  *)
    echo 'FHETEST_GENERATED=2'
    mkdir -p "$FHETEST_RUN_DIR/succ" "$FHETEST_RUN_DIR/fail" "$FHETEST_RUN_DIR/psr_err"
    echo '{"programId":0}' > "$FHETEST_RUN_DIR/succ/0.json" ;;
esac
""")
    executable.chmod(0o755)
    timeout = root / "bin" / "gtimeout"
    timeout.write_text('#!/usr/bin/env bash\nshift 3\nexec "$@"\n')
    timeout.chmod(0o755)
    output = tmp_path / "results"
    command = [
        "bash",
        "-c",
        'export PATH="$1/bin:$PATH"; export EVAL_OUTDIR="$2"; bash "$1/evaluation/run_rq2_rq3.sh" "$3"',
        "test",
        str(root),
        str(output),
        mode,
    ]
    completed = subprocess.run(command, capture_output=True, text=True, check=False)
    assert completed.returncode == 0, completed.stderr
    manifest = next(output.glob("rq2_rq3_run_*.txt")).read_text()
    assert f"MODE={mode}\n" in manifest
    assert "VALID_COUNT_BASIS=generated\n" in manifest
    assert "valid_generated_count_int=2\n" in manifest
    assert "valid_count_int=1\n" in manifest
    assert "random_generated_count_int_1=2\n" in manifest
    assert "-count:2" in (output / "RQ2-random-int-repeat1.command.txt").read_text()
    seeds = [
        line.split("=", 1)[1]
        for line in manifest.splitlines()
        if line.startswith("seed_")
    ]
    assert len(seeds) == len(set(seeds)) == 16
    assert "RUN_FINISHED=1" in manifest
    aggregate(next(output.glob("rq2_rq3_run_*.txt")), output / "aggregated")
    summary = json.loads((output / "aggregated" / "rq2_rq3_summary.json").read_text())
    for row in summary["valid"]:
        assert row["generated"] == 2
        assert row["total"] == 1
        assert row["unrecorded"] == 1
        assert row["succ_rate"] == 0.5
        assert row["recorded_succ_rate"] == 1.0
        assert row["complete"]
    assert len(list(output.glob("*.log"))) == 16
    assert subprocess.run(command, capture_output=True, check=False).returncode != 0
    command[2] = "export FAIL_RUN=1; " + command[2]
    command[-2] = str(tmp_path / "failure")
    failed = subprocess.run(command, capture_output=True, text=True, check=False)
    assert failed.returncode == 7
    failed_manifest = next((tmp_path / "failure").glob("rq2_rq3_run_*.txt")).read_text()
    assert "exit_RQ2-valid-int=7" in failed_manifest
    assert "RUN_FINISHED=1" not in failed_manifest
