FROM clojure:tools-deps-1.11.1.1149

ARG PROBLEM=""
ENV PROBLEM_ENV=$PROBLEM

ARG SEARCH="ga"
ENV SEARCH_ENV=$SEARCH

COPY ./deps.edn /usr/src/cbgp-lite/deps.edn
COPY ./src /usr/src/cbgp-lite/src
COPY ./benchmarks /usr/src/cbgp-lite/benchmarks

WORKDIR /usr/src/cbgp-lite

CMD clj -X:benchmarks erp12.cbgp-lite.benchmark.$SEARCH_ENV/run \
    :suite-ns erp12.cbgp-lite.benchmark.suite.psb \
    :data-dir '"/usr/scr/data/"' \
    :problem "$PROBLEM_ENV"