(ns build
  (:require [clojure.tools.build.api :as b]
             [org.corfield.build :as bb]))

(defn tests
  [opts]
  (bb/run-tests opts))

(defn ci
  [opts]
  (-> opts
      bb/run-tests
      bb/clean
      bb/uber))

(defn build-psb1
  [opts]
  (ci (merge opts {:lib 'erp12/cbgp-lite-psb1
                   :main 'erp12.cbgp-lite.benchmark.psb
                   :basis (b/create-basis {:project "deps.edn"
                                           :aliases [:benchmarks]})
                   :src-dirs ["src" "benchmarks"]})))

;(defn build-psb2
;      [opts]
;      (ci (merge opts {:lib 'erp12/cbgp-lite-psb2
;                       :main 'erp12.cbgp-lite.benchmark.psb2
;                       :basis (b/create-basis {:project "deps.edn"
;                                               :aliases [:benchmarks]})
;                       :src-dirs ["src" "benchmarks"]})))
