# Code Building Genetic Programming

A genetic programming system for synthesizing programs that cover a narrow subset of the Clojure language.

## Project

The repository is organized as follows:

- `src/` — The core CBGP-lite library (`erp12.cbgp-lite.*` namespaces).
- `test/` — Tests mirroring the source namespace structure.
- `benchmarks/` — Problem definitions and a genetic algorithm CLI for benchmark runs.
- `scripts/` — Python and shell utilities for launching local and cloud benchmark runs.
- `deps.edn` — Clojure deps with `:test` and `:benchmarks` aliases.

## Prerequisites

- **Clojure CLI** (`clj`) — required to run the library, tests, and benchmarks. See [clojure.org/guides/install_clojure](https://clojure.org/guides/install_clojure).
- **Java 11+** — Java 21 LTS recommended.
- **Python 3** — required for the scripts in `scripts/`.

## Library

If cbgp-lite is used as a Clojure library the jvm-opt `-Djdk.attach.allowAttachSelf` must be set by the end user.

## Benchmarking

The `benchmarks` folder contains code to run `cbgp-lite` on a suite of standard benchmark problems. These can be
used as end-to-end tests, but are primarily meant to guide research into improvements of the code building GP method.

Benchmark runs can be started in a variety of ways, detailed below.

### Clojure CLI

Invoke the `run` function in `erp12.cbgp-lite.benchmark.ga` using `-X` and the `:benchmarks` alias.

```
clj -X:benchmarks erp12.cbgp-lite.benchmark.ga/run \
  :problem '"vectors-summed"' \
  :data-dir '"data/psb"'
```

The `:problem` argument is the name of the benchmark problem to run. Problems from the PSB2 suite require
`:data-dir` to point to a directory containing the PSB2 data files.

### The `local_runner.py` script

A python script for starting multiple runs, optionally with some number of runs happening in parallel.
Requires Python 3. See `python3 scripts/local_runner.py --help` for possible arguments.

This is the preferred approach to starting multiple runs and capturing a log file for each.

## Development

### Running Tests

To run all tests, run the following command.

```bash
clj -X:test
```

### Linting

This project uses `cljfmt` to lint the codebase. It is assumed `cljfmt` is installed as a [clj Tool](https://clojure.org/reference/clojure_cli#tools).

```bash
clj -Tcljfmt fix
# or
clj -Tcljfmt check
```
