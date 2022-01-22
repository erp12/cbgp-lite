"""Launcher for CBGO runs on the Hampshire College Fly cluster.

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

-h,          --help               Show help message and exit
-n NUM_RUNS, --num-runs NUM_RUNS  The number of runs of the problem to start.
-o OUT,      --out OUT            The path to put the log files of the run captured from stdout.
-d CBGP,     --cbgp CBGP          The path to cbgp-lite.
-m NS,       --ns NS              The namespaces to use as an entrypoint. Must contain -main.
-a ARGS,     --args ARGS          The args to pass through to the main function.
-i ID,       --id ID              The identifier for the overall flight of CBGP runs.
-t TAG,      --tag TAG            An optional tag to add to the runs.

or run `python3 scripts/fly_launcher.py -h` for help.

Example
=======

python3 fly_launcher.py \
    --num-runs 10 \
    --out "~/runs/cbgp/my-experiment/" \
    --cbgp "~/cbgp-lite" \
    --ns "erp12.cbgp-lite.benchmark.psb" \
    --args "~/data/program-synthesis-benchmark-datasets/datasets/ mirror-image"
    --id my-gp-experiment

"""

# @todo Add support for n runs of a set of problems?

import argparse
import os
import subprocess


def alf_cmd(opts: argparse.Namespace, run_id: int) -> str:
    log_file = os.path.join(opts.out, f"run{run_id}.txt")
    cmds = [
        "echo \"Starting run\"",
        "export PATH=$PATH:/usr/java/latest/bin",
        f"cd {opts.cbgp}",
        f"mkdir -p {opts.out}",
        f"/home/erp12/bin/clojure/bin/clojure -M:benchmarks -m {opts.ns} {opts.args} | tee {log_file}",
        "echo \"Finished Run\""
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
    parser.add_argument("-n", "--num-runs", type=int, help="The number of runs of the problem to start.")
    parser.add_argument("-o", "--out", help="The path to put the log files of the run captured from stdout.")
    parser.add_argument("-d", "--cbgp", help="The path to cbgp-lite.")
    parser.add_argument("-m", "--ns", help="The namespaces to use as an entrypoint. Must contain -main.")
    parser.add_argument("-a", "--args", help="The args to pass through to the main function.")
    parser.add_argument("-i", "--id", help="The identifier for the overall flight of CBGP runs.")
    parser.add_argument("-t", "--tag", default="", help="An optional tag to add to the runs.")
    return parser


PIXAR_INIT = "source /etc/sysconfig/pixar"
PIXAR_CMD = "/opt/pixar/tractor-blade-1.7.2/python/bin/python2.6 /opt/pixar/tractor-blade-1.7.2/tractor-spool.py --engine=fly:8000"


if __name__ == "__main__":
    cli_parser = cli_opts()
    args = cli_parser.parse_args()

    args.out = os.path.abspath(os.path.expanduser(args.out))
    args.cbgp = os.path.abspath(os.path.expanduser(args.cbgp))

    if not os.path.isdir(args.cbgp):
        raise ValueError(f"cbgp-lite not found at {args.cbgp}")

    if not os.path.isdir(args.out):
        os.makedirs(args.out)

    alf_file = os.path.join(args.out, "cbgp_runs.alf")
    with open(alf_file, "w") as alf:
        alf.write(alf_job(args))

    ret = subprocess.run(
        f"{PIXAR_INIT};{PIXAR_CMD} {alf_file}",
        shell=True
    )
