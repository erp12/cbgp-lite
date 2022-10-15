problem=$1
search=$2

clj -X:benchmarks erp12.cbgp-lite.benchmark.$search/run \
    :suite-ns erp12.cbgp-lite.benchmark.suite.psb \
    :data-dir '"data/psb/"' \
    :problem "$problem"