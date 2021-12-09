(ns erp12.cbgp-lite.benchmark.odd
  (:require [erp12.cbgp-lite.gp :as gp]))

(defn -main
  [& _]
  ;; @todo Allow option overriding from cli args.
  (gp/run {:input->type {'x int?}
           :return-type boolean?
           :cases [{:inputs [0] :output false}
                   {:inputs [1] :output true}
                   {:inputs [2] :output false}
                   {:inputs [3] :output true}
                   {:inputs [4] :output false}
                   {:inputs [5] :output true}]
           :loss-fn #(if (= %1 %2) 0 1)
           :max-generations 100
           :population-size 100
           ;; @todo Pass genetic source
           ;; @todo Pass breed function?
           })
  (shutdown-agents))
