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
        if p.wait() == 0:
            return p.stdout.read().strip().decode()
        return None


def latest_commit(repo_path: str) -> Optional[str]:
    with Popen(
        ["git", "rev-parse", "--short", "HEAD"], 
        cwd=repo_path, 
        stdout=PIPE, 
        stderr=PIPE,
    ) as p:
        if p.wait() == 0:
            return p.stdout.read().strip().decode()
        return None


def run_cmd(opts: argparse.Namespace, problem: str, run_id: int) -> str:
    branch = active_branch_name(opts.cbgp)
    commit = latest_commit(opts.cbgp)
    log_dir_parts = [opts.out, opts.start_time, branch, commit, problem]
    log_dir = os.path.join(*[part for part in log_dir_parts if part])
    log_file = os.path.join(log_dir, f"run{run_id}.txt")
    main_ns = "erp12.cbgp-lite.benchmark." + opts.search
    types_file = os.path.join(log_dir, f"run{run_id}_types.edn")
    clj_cmd = " ".join([
        f"{opts.clj} -X:benchmarks {main_ns}/run",
        f":suite-ns {opts.suite_ns}",
        # f":data-dir '\"{opts.data_dir}\"'",
        f":problem '\"{problem}\"'",
        f":type-counts-file '\"{types_file}\"'" if opts.log_types else "",
    ] + (opts.opts if opts.opts is not None else []))
    return "; ".join(
        [
            f'echo "Starting run {run_id}"',
            "export PATH=$PATH:/usr/java/latest/bin",
            f"cd {opts.cbgp}",
            f"mkdir -p {log_dir}",
            f"{clj_cmd} 2>&1 | tee {log_file}",
            f'echo "Finished Run {run_id}"',
        ]
    )


def start_run(opts: argparse.Namespace, config: Tuple[str, int]):
    problem, run_id = config
    return run(f"{run_cmd(opts, problem, run_id)}", shell=True)


def cli_opts() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--search",
        default="ga",
        help="Options: ga, random-search, simulated-annealing. Default is ga.",
    )
    parser.add_argument(
        "--suite-ns",
        default="erp12.cbgp-lite.benchmark.suite.psb",
        help="The namespae of the problem suite file which the problem belongs to."
    )
    parser.add_argument(
        "--problems", 
        nargs="+",
        help="The name of the problem(s) in the suite to run."
    )
    parser.add_argument(
        "--data-dir",
        help="The directory to read (and in some cases, download) problem data files to.",
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
        "--log-types", help="If set, an EDN file of type counts will be added to the log file dir.",
        action='store_true',
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

    args.out = os.path.abspath(os.path.expanduser(args.out))
    args.cbgp = os.path.abspath(os.path.expanduser(args.cbgp))
    args.start_time = datetime.now().strftime("%Y%m%d-%H%M%S")

    if not os.path.isdir(args.cbgp):
        raise ValueError(f"cbgp-lite not found at {args.cbgp}")

    if not os.path.isdir(args.out):
        os.makedirs(args.out)

    if args.run_number is not None:
        assert args.num_runs == 1, f"`--run-id` can only be set if `--num-runs` is 1. Got {args.num_runs}."
        assert len(args.problems) == 1, f"`--run-id` can only be set if one problem is provided. Got {args.problems}."
        start_run(args, args.run_number)
    else:
        runs_configs = []
        for problem in args.problems:
            for run_id in range(args.num_runs):
                runs_configs.append((problem, run_id))
        with Pool(args.parallelism) as p:
            for r in p.imap_unordered(partial(start_run, args), runs_configs):
                print(r)

"""
Example:

# Batch of runs
python3 scripts/local_runner.py \
    --search "ga" \
    --suite-ns "erp12.cbgp-lite.benchmark.suite.psb" \
    --problem "vectors-summed" \
    --num-runs 3 \
    --out "./data/logs/" \
    --log-types \
    --opts \
    :data-dir '\"./data/psb\"' \
    :population-size 10 \
    :max-generations 10
    
# Single run
python3 scripts/local_runner.py \
    --search "ga" \
    --problem "vectors-summed" \
    --run-number 1000 \
    --out "./data/logs/" \
    --log-types \
    --opts \
    :data-dir '\"./data/psb\"' \
    :state-output-fn :biggest \
    :population-size 10 \
    :max-generations 10
"""
