#!/usr/bin/env python3
import argparse
import csv
import glob
import json
import os
from typing import Dict, List, Optional, Set, Tuple


def latest_summary_file(root_dir: str) -> Optional[str]:
    patterns = [
        os.path.join(root_dir, "evaluation", "rq2_rq3_run_*.txt"),
        os.path.join(root_dir, "evaluation", "evaluation-*", "rq2_rq3_run_*.txt"),
    ]
    candidates: List[str] = []
    for pattern in patterns:
        candidates.extend(glob.glob(pattern))
    if not candidates:
        return None
    candidates.sort(key=lambda p: os.path.getmtime(p), reverse=True)
    return candidates[0]


def parse_summary_file(path: str) -> Dict[str, str]:
    data: Dict[str, str] = {}
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line or "=" not in line:
                continue
            key, val = line.split("=", 1)
            data[key.strip()] = val.strip()
    return data


def count_json_files(dir_path: str) -> int:
    if not dir_path or not os.path.isdir(dir_path):
        return 0
    count = 0
    for root, _, files in os.walk(dir_path):
        for name in files:
            if name.endswith(".json"):
                count += 1
    return count


def list_json_files(dir_path: str) -> List[str]:
    files: List[str] = []
    if not dir_path or not os.path.isdir(dir_path):
        return files
    for root, _, names in os.walk(dir_path):
        for name in names:
            if name.endswith(".json"):
                files.append(os.path.join(root, name))
    return files


def collect_openfhe_messages(files: List[str]) -> Set[str]:
    messages: Set[str] = set()
    for file_path in files:
        try:
            with open(file_path, "r", encoding="utf-8") as f:
                data = json.load(f)
        except json.JSONDecodeError:
            continue
        results = data.get("results", [])
        for result in results:
            lib = result.get("library")
            msg = result.get("failedResult")
            if lib == "OpenFHE" and msg:
                messages.add(msg)
    return messages


def read_json(path: str) -> Optional[dict]:
    try:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)
    except json.JSONDecodeError:
        return None


def write_rows(path: str, headers: List[str], rows: List[Dict[str, object]]) -> None:
    with open(path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=headers)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def write_valid_fail_csv(run_dir: str, out_path: str) -> None:
    fail_dir = os.path.join(run_dir, "fail")
    files = list_json_files(fail_dir)
    rows: List[Dict[str, object]] = []
    for path in files:
        data = read_json(path)
        if not data:
            continue
        program_id = data.get("programId")
        failures = data.get("failures", [])
        openfhe = ""
        for result in failures:
            if result.get("library") == "OpenFHE":
                openfhe = result.get("failedResult") or ""
        rows.append({"programId": program_id, "OpenFHE": openfhe})
    rows.sort(key=lambda r: int(r["programId"]))
    write_rows(out_path, ["programId", "OpenFHE"], rows)


def write_invalid_exception_csv(run_dirs: List[str], out_path: str, limit: Optional[int]) -> None:
    files: List[str] = []
    for run_dir in run_dirs:
        exception_dir = os.path.join(run_dir, "exception")
        files.extend(list_json_files(exception_dir))
    files.sort()
    if limit is not None and limit > 0:
        files = files[:limit]
    rows: List[Dict[str, object]] = []
    for path in files:
        data = read_json(path)
        if not data:
            continue
        program_id = data.get("programId")
        results = data.get("results", [])
        seal = ""
        openfhe = ""
        for result in results:
            if result.get("library") == "SEAL":
                seal = result.get("failedResult") or ""
            elif result.get("library") == "OpenFHE":
                openfhe = result.get("failedResult") or ""
        rows.append({"programId": program_id, "SEAL": seal, "OpenFHE": openfhe})
    rows.sort(key=lambda r: int(r["programId"]))
    write_rows(out_path, ["programId", "SEAL", "OpenFHE"], rows)


def summarize_valid(run_dir: Optional[str]) -> Dict[str, int]:
    if not run_dir:
        return {"succ": 0, "fail": 0, "psr_err": 0, "total": 0}
    succ = count_json_files(os.path.join(run_dir, "succ"))
    fail = count_json_files(os.path.join(run_dir, "fail"))
    psr_err = count_json_files(os.path.join(run_dir, "psr_err"))
    total = succ + fail + psr_err
    return {"succ": succ, "fail": fail, "psr_err": psr_err, "total": total}


def summarize_invalid(run_dirs: List[str], limit: Optional[int] = None) -> Dict[str, int]:
    if not run_dirs:
        return {"exceptions": 0, "unique": 0, "expected": 0, "unexpected": 0}
    exception_files: List[str] = []
    expected_files: List[str] = []
    unexpected_files: List[str] = []
    for run_dir in run_dirs:
        if not run_dir:
            continue
        exception_dir = os.path.join(run_dir, "exception")
        expected_dir = os.path.join(exception_dir, "expected")
        unexpected_dir = os.path.join(exception_dir, "unexpected")
        exception_files.extend(list_json_files(exception_dir))
        expected_files.extend(list_json_files(expected_dir))
        unexpected_files.extend(list_json_files(unexpected_dir))
    exception_files.sort()
    expected_files.sort()
    unexpected_files.sort()

    if limit is not None and limit > 0:
        exception_files = exception_files[:limit]
        # expected/unexpected are subsets of exception_dir; limit by file membership
        exception_set = set(exception_files)
        expected_files = [f for f in expected_files if f in exception_set]
        unexpected_files = [f for f in unexpected_files if f in exception_set]

    unique = len(collect_openfhe_messages(exception_files))
    return {
        "exceptions": len(exception_files),
        "unique": unique,
        "expected": len(expected_files),
        "unexpected": len(unexpected_files),
    }


def main() -> None:
    repo_root = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
    default_summary = latest_summary_file(repo_root)

    parser = argparse.ArgumentParser()
    parser.add_argument("--summary", default=default_summary, help="Summary file from run_rq2_rq3.sh")
    parser.add_argument("--outdir", default=None, help="Output directory for CSV/JSON")
    args = parser.parse_args()

    if not args.summary or not os.path.isfile(args.summary):
        raise SystemExit("Summary file not found. Provide --summary explicitly.")

    summary = parse_summary_file(args.summary)
    outdir = args.outdir or summary.get("eval_outdir") or os.path.dirname(os.path.abspath(args.summary))
    os.makedirs(outdir, exist_ok=True)

    runs: List[Tuple[str, str, Optional[str]]] = [
        ("int", "valid", summary.get("valid_dir_int")),
        ("int", "random", summary.get("random_dir_int")),
        ("double", "valid", summary.get("valid_dir_double")),
        ("double", "random", summary.get("random_dir_double")),
    ]

    valid_rows: List[Dict[str, object]] = []
    for enc, kind, run_dir in runs:
        stats = summarize_valid(run_dir)
        total = stats["total"]
        succ = stats["succ"]
        rate = (succ / total) if total else 0.0
        valid_rows.append(
            {
                "encType": enc,
                "kind": kind,
                "runDir": run_dir or "",
                "total": total,
                "succ": succ,
                "fail": stats["fail"],
                "psr_err": stats["psr_err"],
                "succ_rate": f"{rate:.4f}",
            }
        )

    def parse_dirs(key: str) -> List[str]:
        if key in summary:
            val = summary.get(key, "")
            return [p for p in val.split(",") if p]
        single = summary.get(key.replace("dirs_", "dir_"), "")
        return [single] if single else []

    invalid_runs: List[Tuple[str, str, List[str], Optional[int]]] = [
        ("int", "invalid", parse_dirs("invalid_dirs_int"), None),
        ("int", "invalid_random", parse_dirs("invalid_random_dirs_int"),
         int(summary.get("invalid_random_exc_target_int", "0")) or None),
        ("double", "invalid", parse_dirs("invalid_dirs_double"), None),
        ("double", "invalid_random", parse_dirs("invalid_random_dirs_double"),
         int(summary.get("invalid_random_exc_target_double", "0")) or None),
    ]

    invalid_rows: List[Dict[str, object]] = []
    for enc, kind, run_dirs, limit in invalid_runs:
        stats = summarize_invalid(run_dirs, limit if kind == "invalid_random" else None)
        invalid_rows.append(
            {
                "encType": enc,
                "kind": kind,
                "runDir": ",".join(run_dirs),
                "exceptions": stats["exceptions"],
                "unique_messages": stats["unique"],
                "expected": stats["expected"],
                "unexpected": stats["unexpected"],
            }
        )

    valid_csv = os.path.join(outdir, "rq2_valid_summary.csv")
    invalid_csv = os.path.join(outdir, "rq2_rq3_invalid_summary.csv")
    json_out = os.path.join(outdir, "rq2_rq3_summary.json")

    write_rows(
        valid_csv,
        ["encType", "kind", "runDir", "total", "succ", "fail", "psr_err", "succ_rate"],
        valid_rows,
    )
    write_rows(
        invalid_csv,
        ["encType", "kind", "runDir", "exceptions", "unique_messages", "expected", "unexpected"],
        invalid_rows,
    )

    with open(json_out, "w", encoding="utf-8") as f:
        json.dump(
            {
                "summary_file": args.summary,
                "valid": valid_rows,
                "invalid": invalid_rows,
            },
            f,
            indent=2,
        )

    print(f"Summary: {args.summary}")
    print(f"Wrote: {valid_csv}")
    print(f"Wrote: {invalid_csv}")
    print(f"Wrote: {json_out}")

    # Legacy-format CSV outputs (matching evaluation-202505 scripts)
    legacy_files: List[str] = []

    # RQ2-1-a: valid (filterOn) and random (filterOff)
    if summary.get("valid_dir_int"):
        out = os.path.join(outdir, "output-rq2-1-a-filterOn-int.csv")
        write_valid_fail_csv(summary.get("valid_dir_int", ""), out)
        legacy_files.append(out)
    if summary.get("valid_dir_double"):
        out = os.path.join(outdir, "output-rq2-1-a-filterOn-double.csv")
        write_valid_fail_csv(summary.get("valid_dir_double", ""), out)
        legacy_files.append(out)
    if summary.get("random_dir_int"):
        out = os.path.join(outdir, "output-rq2-1-a-filterOff-int.csv")
        write_valid_fail_csv(summary.get("random_dir_int", ""), out)
        legacy_files.append(out)
    if summary.get("random_dir_double"):
        out = os.path.join(outdir, "output-rq2-1-a-filterOff-double.csv")
        write_valid_fail_csv(summary.get("random_dir_double", ""), out)
        legacy_files.append(out)

    # RQ2-1-b: invalid (filterOn) and invalid random (filterOff)
    if summary.get("invalid_dir_int"):
        out = os.path.join(outdir, "output-rq2-1-b-filterOn-int.csv")
        write_invalid_exception_csv([summary.get("invalid_dir_int", "")], out, None)
        legacy_files.append(out)
    if summary.get("invalid_dir_double"):
        out = os.path.join(outdir, "output-rq2-1-b-filterOn-double.csv")
        write_invalid_exception_csv([summary.get("invalid_dir_double", "")], out, None)
        legacy_files.append(out)

    invalid_random_dirs_int = parse_dirs("invalid_random_dirs_int")
    invalid_random_dirs_double = parse_dirs("invalid_random_dirs_double")
    invalid_random_limit_int = int(summary.get("invalid_random_exc_target_int", "0")) or None
    invalid_random_limit_double = int(summary.get("invalid_random_exc_target_double", "0")) or None

    if invalid_random_dirs_int:
        out = os.path.join(outdir, "output-rq2-1-b-filterOff-int.csv")
        write_invalid_exception_csv(invalid_random_dirs_int, out, invalid_random_limit_int)
        legacy_files.append(out)
    if invalid_random_dirs_double:
        out = os.path.join(outdir, "output-rq2-1-b-filterOff-double.csv")
        write_invalid_exception_csv(invalid_random_dirs_double, out, invalid_random_limit_double)
        legacy_files.append(out)

    for path in legacy_files:
        print(f"Wrote: {path}")


if __name__ == "__main__":
    main()
