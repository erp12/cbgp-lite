FROM clojure:tools-deps-1.11.1.1149

ARG P=""
ENV PROBLEM=$P
ENV MAIN_NAMESPACE="erp12.cbgp-lite.benchmark.ga"

COPY ./deps.edn /usr/src/cbgp-lite/deps.edn
COPY ./src /usr/src/cbgp-lite/src
COPY ./benchmarks /usr/src/cbgp-lite/benchmarks

WORKDIR /usr/src/cbgp-lite

CMD clj -X:benchmarks $MAIN_NAMESPACE/run \
    :suite-ns erp12.cbgp-lite.benchmark.suite.psb \
    :data-dir '"/usr/scr/data/"' \
    :problem "$PROBLEM"