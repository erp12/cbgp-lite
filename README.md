# Code Building GP - Lite

A genetic programming system for synthesizing programs that cover a narrow subset of the Clojure language.

## Benchmarking

The `benchmarks` folder contains code to run `cbgp-lite` on a suite of standard benchmark problems. These can be 
used as end-to-end tests, but are primarily meant to guide research into improvements of the code building GP method.

Benchmark runs can be stared in a variety of ways, detailed below.

### Clojure CLI

Simply invoke the `run` function in one of the benchmark method repos using `-X` and the `:benchmarks` alias.

The `:suite-ns` argument denotes the namespace of the problem suite which will serve training and test cases via
a `read-cases` function. It also must a `problems` function that will return a map of problem metadata 
(types, literals, ERC generators, error functions).

The remaining arguments should be whatever is required by both the `run` function of the main namespace 
and the `read-cases` function of the `:suite-ns`. 

```
clj -X:benchmarks erp12.cbgp-lite.benchmark.ga/run \
  :suite-ns erp12.cbgp-lite.benchmark.suite.psb \
  :data-dir '"data/program-synthesis-benchmark-datasets/datasets"' \
  :problem $1
```

This is the preferred approach for starting a single run.

### The `local_runner.py` script

A python script for starting multiple runs, optionally with some number of runs happening in parallel. 
Requires Python 3. See `python3 scripts/local_runner.py --help` for possible arguments.

### Docker

The `scripts/build.sh` script makes it easy to build Docker images that will perform a single run of
a specific PSB suite problem. It takes a single argument: the problem name. The resulting image will 
be named `fcbgp-[problem]` where `[problem]` is the problem name.

These images can be run to create a container that will perform a single run.

## Development

### Running Tests

```text
clj -T:build tests
```

## To-Do

Remove git submodule and ignore `data/` now that PSB1 problems are provided by `psb2-clojure`.