"""Launcher for fCBGP runs on the Hampshire College Fly cluster.

Setup
=====

1. Get access to an account on the Fly cluster.
2. SSH into the Fly cluster.
3. Clone `cbgp-lite` (probably your personal fork) in your user directory.
4. Make sure that your have Python 3.6 or later on your path.

How To Run
==========

1. Put your version of `cbgp-lite` on the correct version you would like to run. For example, checkout your
feature branch.

2. Run this script with Python 3 with `python3 scripts/fly_launcher [PARAMS]`.

Parameters
==========

--help               Show help message and exit
--search SEARCH      The search algorithm. Options: [ga, random-search, simulated-annealing]. Default is ga.
--data-dir DATA_DIR  The directory to read (and in some cases, download) problem data files to.
--num-runs NUM_RUNS  The number of runs of the problem to start.
--out OUT            The path to put the log files of the run captured from stdout.
--cbgp CBGP          The path to cbgp-lite.
--id ID              The identifier for the overall flight of CBGP runs.
--tag TAG            An optional tag to add to the runs.

or run `python3 scripts/fly_launcher.py -h` for help.

Example
=======

python3 scripts/fly_launcher.py \
    --search "ga" \
    --problem "replace-space-with-newline" \
    --data-dir "./data/psb/" \
    --num-runs 3 \
    --out "~/runs/cbgp/my-experiment/" \
    --id my-gp-experiment \
    --dry-run

"""

import argparse
import os
import subprocess
from datetime import datetime

CLJ = "/home/erp12/bin/clojure/bin/clojure"


def alf_cmd(opts: argparse.Namespace, run_id: int) -> str:
    log_dir = os.path.join(opts.out, opts.start_time, opts.problem)
    log_file = os.path.join(log_dir, f"run{run_id}.txt")
    main_ns = "erp12.cbgp-lite.benchmark." + opts.search
    suite_ns = "erp12.cbgp-lite.benchmark.suite.psb"
    cmds = [
        'echo "Starting run"',
        "export PATH=$PATH:/usr/java/latest/bin",
        f"cd {opts.cbgp}",
        f"mkdir -p {log_dir}",
        f"{CLJ} -X:benchmarks {main_ns}/run :suite-ns {suite_ns} :data-dir '\"{opts.data_dir}\"' :problem '\"{opts.problem}\"' 2>&1 | tee {log_file}",
        'echo "Finished Run"',
    ]
    return f"""RemoteCmd {{/bin/sh -c {{{"; ".join(cmds)}}}}}"""


def alf_task(opts: argparse.Namespace, run_id: int) -> str:
    title = f"Run {run_id} - {opts.id}"
    return f"""Task -title {{{title}}} -cmds {{
        {alf_cmd(opts, run_id)} -service {{tom}} -tags {{{opts.tag}}}
    }}
"""


def alf_job(opts: argparse.Namespace) -> str:
    tasks = "\n    ".join([alf_task(opts, i) for i in range(opts.num_runs)])
    return f"""##AlfredToDo 3.0
Job -title {{{opts.id}}} -subtasks {{
    {tasks}
}}
"""


def cli_opts() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--search",
        default="ga",
        help="The search algorithm. Options: [ga, random-search, simulated-annealing]. Default is ga."
    )
    parser.add_argument("--problem", help="The name of the problem to run.")
    parser.add_argument(
        "--data-dir",
        help="The directory to read (and in some cases, download) problem data files to.",
    )
    parser.add_argument(
        "--num-runs", type=int, help="The number of runs of the problem to start."
    )
    parser.add_argument(
        "--out", help="The path to put the log files of the run captured from stdout."
    )
    parser.add_argument("--id", help="The identifier for the overall flight of runs.")
    parser.add_argument("--cbgp", help="The path to cbgp-lite.", default=".")
    parser.add_argument(
        "--tag", default="4pernode", help="An optional tag to add to the runs."
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="If set, will write but not submit the ALF file.",
    )
    return parser


PIXAR_INIT = "source /etc/sysconfig/pixar"
PIXAR_CMD = "/opt/pixar/tractor-blade-1.7.2/python/bin/python2.6 /opt/pixar/tractor-blade-1.7.2/tractor-spool.py --engine=fly:8000"


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

    alf_file = os.path.join(args.out, "cbgp_runs.alf")
    with open(alf_file, "w") as alf:
        alf.write(alf_job(args))

    if not args.dry_run:
        try:
            subprocess.run(
                f"{PIXAR_INIT};{PIXAR_CMD} {alf_file}",
                shell=True,
                stderr=subprocess.STDOUT,
            )
        except subprocess.CalledProcessError as e:
            print("Encountered error!")
            print(e.output)
