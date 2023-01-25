import argparse
import os
from datetime import datetime
import subprocess


def run_cmd(opts: argparse.Namespace, run_id: int) -> str:
    log_dir = os.path.join(opts.out, opts.problem)
    log_file = os.path.join(log_dir, f"run{run_id}.txt")
    main_ns = "erp12.cbgp-lite.benchmark." + opts.search
    suite_ns = "erp12.cbgp-lite.benchmark.suite.psb"
    types_file = os.path.join(log_dir, f"run{run_id}_types.edn")

    clj_cmd = " ".join([
        f"{opts.clj} -X:benchmarks {main_ns}/run",
        f":suite-ns {suite_ns}",
        f":data-dir '\"{opts.data_dir}\"'",
        f":problem '\"{opts.problem}\"'",
        f":state-output-fn {opts.ast_strategy}" if opts.ast_strategy is not None else "",
        f":type-counts-file '\"{types_file}\"'" if opts.log_types else "",
    ])

    command = "; ".join(
        [
            f'echo "Starting run {run_id}"',
            "export PATH=$PATH:/usr/java/latest/bin",
            f"cd {opts.cbgp}",
            f"mkdir -p {log_dir}",
            f"{clj_cmd} 2>&1 | tee {log_file}",
            f'echo "Finished Run {run_id}"',
        ]
    )

    print("FULL COMMAND ON NEXT LINE")
    print(command)
    return command


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
        "--run-number", type=int, help="The number of this run."
    )
    parser.add_argument(
        "--out", help="The path to put the log files of the run captured from stdout."
    )
    parser.add_argument(
        "--ast-strategy",
        default=None,
        help="The method of selecting and AST post-compilation.",
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

    start_run(args, args.run_number)

"""
Example:

python3 scripts/single_run_runner.py \
    --search "ga" \
    --problem "vectors-summed" \
    --data-dir "./data/psb/" \
    --run-number 42 \
    --out "./data/logs/test/" \
    --ast-strategy :biggest-out \
    --log-types
"""
