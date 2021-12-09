# Code Building GP - Lite

A genetic programming system for synthesizing programs that cover a narrow subset of the Clojure language.

## To-Do

- Fill out `lib.clj` with full set of functions
- plushy genomes
- Implement benchmark problems
- Compare runtime of single-stack vs multi-stack
- Generalization testing
- Genome simplification

## Running Tests

```text
clj -T:build tests
```

## Running Benchmark Problem

```text
clj -M:benchmarks -m erp12.cbgp-lite.benchmark.odd
```
