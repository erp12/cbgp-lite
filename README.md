# Code Building GP - Lite

A genetic programming system for synthesizing programs that cover a narrow subset of the Clojure language.

## Benchmarking

The `benchmarks` folder contains code to run `cbgp-lite` on a suite of standard benchmark problems. These can be 
used as end-to-end tests, but are primarily meant to guide research into improvements of the code building GP method.

Benchmark runs can be started in a variety of ways, detailed below.

### Clojure CLI

Simply invoke the `run` function in one of the benchmark method repos using `-X` and the `:benchmarks` alias.

The main namespaces depends on the search algorithm you want to use. For all namespaces the entrypoint is the `run`
funciton. Currently supported search algorithm namespaces are as follows:

- `erp12.cbgp-lite.benchmark.ga`
- `erp12.cbgp-lite.benchmark.simulated-annealing`
- `erp12.cbgp-lite.benchmark.hill-climbing`
- `erp12.cbgp-lite.benchmark.random-search`

The `:suite-ns` argument denotes the namespace of the problem suite which will serve training and test cases via
a `read-cases` function. It must also have a `problems` function that will return a map of problem metadata 
(types, literals, ERC generators, error functions) when given a map of options.

The remaining arguments should be whatever is required by both the `run` function of the main namespace 
and the `read-cases` function of the `:suite-ns`. 

```
clj -X:benchmarks erp12.cbgp-lite.benchmark.ga/run \
  :suite-ns erp12.cbgp-lite.benchmark.suite.psb \
  :data-dir '"data/psb"' \
  :problem "vectors-summed"
```

This is the preferred approach for starting a single run.

### The `local_runner.py` script

A python script for starting multiple runs, optionally with some number of runs happening in parallel. 
Requires Python 3. See `python3 scripts/local_runner.py --help` for possible arguments.

This is the preferred approach to starting multiple runs and capturing a log file for each. 

### Docker

The `scripts/build.sh` script makes it easy to build Docker images that will perform a single run of
a specific PSB suite problem. It takes a two arguments:

- The search algorithm to use. Choices are `ga`, `simulated-annealing`, `hill-climbing`, `random-search`.
- The problem name.

The resulting image will be named `fcbgp-[search]-[problem]` where `[search]` and `[problem]` are the arguments
provided to the build script.

Running these images in a container will perform a single run.

## Development

### Running Tests

```text
clj -X:test
```
