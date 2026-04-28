(ns erp12.cbgp-lite.benchmark.problems.odd
  "Benchmark problem definition for the simple odd-number predicate (int -> bool)."
  (:require [erp12.cbgp-lite.genome :as g]
            [erp12.cbgp-lite.program.types :as t]))

(def penalty 2)

(defn ->case 
  [x]
  {:inputs [x] :output (odd? x)})

(def odd-problem
  {:input-symbols  ['x]
   :input-types    [t/INT]
   :output-type    t/BOOL
   :type-ctors     #{t/INT t/BOOL}
   :extra-genes    [(g/->Lit true t/BOOL)
                    (g/->Lit false t/BOOL)
                    (g/->Lit 0 t/INT)
                    (g/->Lit 1 t/INT)
                    (g/->Lit 2 t/INT)]
   :dataset-reader (fn [{:keys [n-train n-test]}]
                      {:train (map ->case (range n-train))
                       :test  (map ->case (range n-test))})
   :penalty        penalty
   :loss-fns       [(fn [actual expected]
                      (cond 
                        (nil? actual) penalty
                        (= actual expected) 0
                        :else 1))]})