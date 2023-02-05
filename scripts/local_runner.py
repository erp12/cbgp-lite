import argparse
import os
from datetime import datetime
from functools import partial
import subprocess
from multiprocessing import Pool


SUITE_NS = "erp12.cbgp-lite.benchmark.suite.psb"


def run_cmd(opts: argparse.Namespace, run_id: int) -> str:
    log_dir = os.path.join(opts.out, opts.start_time, opts.problem)
    log_file = os.path.join(log_dir, f"run{run_id}.txt")
    main_ns = "erp12.cbgp-lite.benchmark." + opts.search
    types_file = os.path.join(log_dir, f"run{run_id}_types.edn")
    clj_cmd = " ".join([
        f"{opts.clj} -X:benchmarks {main_ns}/run",
        f":suite-ns {SUITE_NS}",
        f":data-dir '\"{opts.data_dir}\"'",
        f":problem '\"{opts.problem}\"'",
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


def start_run(opts: argparse.Namespace, run_id: int):
    return subprocess.run(f"{run_cmd(opts, run_id)}", shell=True)


def cli_opts() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--search",
        default="ga",
        help="Options: ga, random-search, simulated-annealing. Default is ga.",
    )
    parser.add_argument("--problem", help="The name of the problem to run.")
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
        help="The number of runs of the problem to start. Default is 1."
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
        assert args.num_runs == 1, "`--run-id` can only be set if `--num-runs` is 1."
        start_run(args, args.run_number)
    else:
        with Pool(args.parallelism) as p:
            for r in p.imap_unordered(partial(start_run, args), range(args.num_runs)):
                print(r)

"""
Example:

# Batch of runs
python3 scripts/local_runner.py \
    --search "ga" \
    --problem "vectors-summed" \
    --data-dir "./data/psb/" \
    --num-runs 3 \
    --out "./data/logs/" \
    --log-types \
    --opts \
    :state-output-fn :biggest \
    :population-size 10 \
    :max-generations 10
    
# Single run
python3 scripts/local_runner.py \
    --search "ga" \
    --problem "vectors-summed" \
    --data-dir "./data/psb/" \
    --run-number 1000 \
    --out "./data/logs/" \
    --log-types \
    --opts \
    :state-output-fn :biggest \
    :population-size 10 \
    :max-generations 10
"""
