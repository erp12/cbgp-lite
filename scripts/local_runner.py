import argparse
import os
from functools import partial
import subprocess
from multiprocessing import Pool, cpu_count


def run_cmd(opts: argparse.Namespace, run_id: int) -> str:
    log_file = os.path.join(opts.out, f"run{run_id}.txt")
    return "; ".join([
        f"echo \"Starting run {run_id}\"",
        "export PATH=$PATH:/usr/java/latest/bin",
        f"cd {opts.cbgp}",
        f"mkdir -p {opts.out}",
        f"{opts.clj} -M:benchmarks -m {opts.ns} {opts.args} | tee {log_file}",
        f"echo \"Finished Run {run_id}\""
    ])


def start_run(opts: argparse.Namespace, run_id: int):
    return subprocess.run(
        f"{run_cmd(opts, run_id)}",
        shell=True
    )


def cli_opts() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument("-n", "--num-runs", type=int, help="The number of runs of the problem to start.")
    parser.add_argument("-o", "--out", help="The path to put the log files of the run captured from stdout.")
    parser.add_argument("-c", "--clj", help="Path to the clojure CLI binary.")
    parser.add_argument("-d", "--cbgp", help="The path to cbgp-lite.")
    parser.add_argument("-m", "--ns", help="The namespaces to use as an entrypoint. Must contain -main.")
    parser.add_argument("-a", "--args", help="The args to pass through to the main function.")
    parser.add_argument("-p", "--parallelism", type=int, default=cpu_count(), help="The number of runs that can be running concurrently.")
    return parser


if __name__ == "__main__":
    cli_parser = cli_opts()
    args = cli_parser.parse_args()

    args.out = os.path.abspath(os.path.expanduser(args.out))
    args.cbgp = os.path.abspath(os.path.expanduser(args.cbgp))

    if not os.path.isdir(args.cbgp):
        raise ValueError(f"cbgp-lite not found at {args.cbgp}")

    if not os.path.isdir(args.out):
        os.makedirs(args.out)

    with Pool(args.parallelism) as p:
        for r in p.imap_unordered(partial(start_run, args), range(args.num_runs)):
            print(r)

"""
Example:

python3 scripts/local_runner.py \
    --num-runs 3 \
    --out "./data/logs/test/" \
    --clj "/usr/local/bin/clojure" \
    --cbgp "." \
    --ns "erp12.cbgp-lite.benchmark.psb" \
    --args "data/program-synthesis-benchmark-datasets/datasets replace-space-with-newline" \
    --parallelism 5
"""
