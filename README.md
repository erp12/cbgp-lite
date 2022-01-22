# Code Building GP - Lite

A genetic programming system for synthesizing programs that cover a narrow subset of the Clojure language.

## To-Do

- plushy genomes
- Compare runtime of single-stack vs multi-stack
- Genome simplification

## Running Tests

```text
clj -T:build tests
```

## Running Benchmark Problem

```text
clj -M:benchmarks -m erp12.cbgp-lite.benchmark.odd
```
