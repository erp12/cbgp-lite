from typing import Tuple, Optional
import argparse
import os
from datetime import datetime
from functools import partial
from subprocess import run, Popen, PIPE
from multiprocessing import Pool


def active_branch_name(repo_path: str) -> Optional[str]:
    with Popen(
        ["git", "rev-parse", "--abbrev-ref", "HEAD"],
        cwd=repo_path, 
        stdout=PIPE, 
        stderr=PIPE,
    ) as p:
        if p.wait() == 0 and p.stdout:
            return p.stdout.read().strip().decode()
        return None


def latest_commit(repo_path: str) -> Optional[str]:
    with Popen(
        ["git", "rev-parse", "--short", "HEAD"], 
        cwd=repo_path, 
        stdout=PIPE, 
        stderr=PIPE,
    ) as p:
        if p.wait() == 0 and p.stdout:
            return p.stdout.read().strip().decode()
        return None


def run_cmd(opts: argparse.Namespace, problem: str, run_id: int) -> str:
    branch = active_branch_name(opts.cbgp)
    commit = latest_commit(opts.cbgp)
    log_dir = None
    log_file = None
    if opts.out:
        log_dir = os.path.join(opts.out, f"{branch=}", f"{commit=}", f"started={opts.start_time}", f"{problem=}")
        log_file = os.path.join(log_dir, f"run{run_id}.txt")
    clj_cmd = " ".join([
        f"{opts.clj} -X:benchmarks erp12.cbgp.benchmark.ga/run",
        f":problem '\"{problem}\"'",
        f":run-id '\"{run_id}\"'",
    ] + (opts.opts if opts.opts is not None else []))
    return "; ".join(
        [
            f'echo "Starting run {run_id}"',
            "export PATH=$PATH:/usr/java/latest/bin",
            f"cd {opts.cbgp}",
            f"mkdir -p {log_dir}" if log_dir else "echo \"Not creating a log file.\"",
            f"{clj_cmd} 2>&1" + (f" | tee {log_file}" if log_file else ""),
            f'echo "Finished Run {run_id}"',
        ]
    )


def start_run(opts: argparse.Namespace, config: Tuple[str, int]):
    problem, run_id = config
    return run(f"{run_cmd(opts, problem, run_id)}", shell=True)


def cli_opts() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--problems", 
        nargs="+",
        help="The name of the problem(s) in the suite to run."
    )
    parser.add_argument(
        "--run-number",
        type=int,
        default=None,
        help="""An identifier for the single run. If None (the default) run numbers will be automatically generated.
         NOTE: `--num-runs` must be 1 if a `--run-number` is supplied. """
    )
    parser.add_argument(
        "--num-runs",
        type=int,
        default=1,
        help="The number of runs per problem to start. Default is 1."
    )
    parser.add_argument(
        "--out", help="The path to put the log files of the run captured from stdout."
    )
    parser.add_argument(
        "--clj",
        help="Path to the clojure CLI binary.",
        default="/usr/local/bin/clojure",
    )
    parser.add_argument("--cbgp", help="The path to cbgp-lite.", default=".")
    parser.add_argument(
        "--parallelism",
        type=int,
        default=1,
        help="The number of runs to perform concurrently. If runs are multi-threaded, recommend this be 1. Default is 1.",
    )
    parser.add_argument("--opts", nargs="*")
    return parser


if __name__ == "__main__":
    cli_parser = cli_opts()
    args = cli_parser.parse_args()

    if args.out:
        args.out = os.path.abspath(os.path.expanduser(args.out))
    args.cbgp = os.path.abspath(os.path.expanduser(args.cbgp))
    args.start_time = datetime.now().strftime("%Y%m%d-%H%M%S")

    if not os.path.isdir(args.cbgp):
        raise ValueError(f"cbgp-lite not found at {args.cbgp}")

    if args.out and not os.path.isdir(args.out):
        os.makedirs(args.out)

    runs_configs = []
    if args.run_number is not None:
        assert args.num_runs == 1, f"`--run-id` can only be set if `--num-runs` is 1. Got {args.num_runs}."
        assert len(args.problems) == 1, f"`--run-id` can only be set if one problem is provided. Got {args.problems}."
        start_run(args, (args.problems[0], args.run_number))
    else:
        for problem in args.problems:
            for run_id in range(args.num_runs):
                runs_configs.append((problem, run_id))
        with Pool(args.parallelism) as p:
            for r in p.imap_unordered(partial(start_run, args), runs_configs):
                print(r)

"""
Examples:

# Batch of runs
python3 scripts/local_runner.py \
    --problems "vectors-summed" \
    --num-runs 3 \
    --out "./data/logs/" \
    --opts \
    :data-dir '\"./data/psb\"'
    
# Single run
python3 scripts/local_runner.py \
    --problems "vectors-summed" \
    --run-number 1000 \
    --out "./data/logs/" \
    --opts \
    :data-dir '\"./data/psb\"' \
    :state-output-fn :biggest \
    :population-size 10 \
    :max-generations 10


python3 scripts/local_runner.py \
    --problems "compare-string-lengths" \
    --num-runs 1 \
    --out "./data/logs/" \
    --opts \
    :data-dir '\"./data/psb\"'

"""
