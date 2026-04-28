# CLAUDE.md

Guidelines for agentic coding assistants working on this repository.

## Project Overview

CBGP-lite implements Code Building Genetic Programming in Clojure for program synthesis.

- `erp12.cbgp-lite.*` - The CBGP library.
- `erp12.cbgp-experimental.*` - Experimental code that is not actively developed and should be ignored by agents.
- The `benchmarks/` directory contains an implementation of benchmark problems and a CLI for starting runs.
- The `test/` directory contains test with a similar namespace pattern.

## Build, Lint, Test Commands

### Running Tests

```bash
clj -X:test # All tests
clj -X:test :only '"test/erp12/cbgp/"' # Specific directory
clj -X:test :only erp12.cbgp-lite.program.types-test  # Specific namespace
clj -X:test :only 'erp12.cbgp-lite.program.types-test/fail-test'  # Specific test
```

### Lint

```bash
clj -Tcljfmt fix
```

### Git

Do not create any git commits or issue any git commands that would impact the commit history.

## Code Style

### Namespace Structure

Pattern: `erp12.cbgp-lite.*`

### Imports

```clojure
(ns erp12.cbgp-lite.program.expr
  "Records and functions for constructing expressions ASTs, inferring expression types, and exploring expression structures."
  (:require [clojure.walk :refer [postwalk]]
            [erp12.cbgp-lite.program.lib :refer [guard]]
            [erp12.cbgp-lite.program.types :as t])
  (:import (clojure.lang Compiler$CompilerException)))
```

- Sort alphabetically, group with blank lines
- Use `:import` for Java classes and exceptions
- `:refer` for frequently used symbols, qualified names otherwise

### Records

```clojure
(defrecord Var [sym])
(defrecord Lit [val typ])
(defrecord Ast [expr typ])
```

### Naming

- Variables/Functions: kebab-case (`infer-type`, `safe-div`)
- Records: PascalCase (`TypeVar`, `TypeConstructor`)
- Constants: screaming-snake-case (`DEFAULT-PENALTY`)
- Private: `defn-` macro (`(defn- helper-fn ...)`)
- Predicates: Trailing `?` (`kind-fn?`, `unifiable?`)
- Conversion: `->` prefix (`->vector1`)

### Comments

- Namespace/docstrings describe purpose
- No inline comments for obvious code
- Use docstrings for public functions

### Testing

- `deftest`, `is`, `testing` from `clojure.test`
- `matcher-combinators` for pattern matching
- Import `ExceptionInfo` for exception tests
- Test success/failure cases, edge cases

## Key Components

### Genomes (genome.clj)

Plushy genomes are sequential data structures containing gene objects. 
Plushy genomes are randomly generated and mutated during evolution.
Plushy genomes are transformed into executable Clojure functions after translation into Push code and then compiled to Clojure using a stack machine.

Gene Types: `Var`, `Local`, `Lit`, `LitGenerator`, `App`, `Abs`, `Let`, `:close`

Genetic source: A function that samples genes using a fixed gene-type distribution.

### Type System (types.clj)

Hindley-Milner type inference with TypeVar, TypeConstructor, TypeApp, Scheme.

### Compiler (compile.clj)

A stack machine that compiles Push code (from a translated plushy genome) into type-safe Clojure forms.

The stack holds AST records containing 2 fields: `:expr` is a lambda calculus expression, and `:typ` the data type of the expression.

- `locals` is vector of symbols denoting the local variables that are in-scope for that call to the compiler.
- `type-env` is a type environment map of symbols to their data types.
- `hooks` is a map of callback functions called at different points to use for collecting compilation statistics and debugging.

### Individuals (individual.clj)

Code for creating fully compiled and evaluated genomes during evolution. 
Defines a Clojure function for the genome after stack-machine compilation and then runs the function on a batch of training cases.
Gathers the errors on each training cases uses a collection of loss functions, penalizing exceptions.
Individuals are a map with `:push`, `:code`, `:func`, `:behavior`, `:errors`, `:total-error`, `:solution?`, `:cases-used`, `:exception`, `:exceeded-mem`
