(ns erp12.cbgp-lite.benchmark.problems
  "Registry of all benchmark problems and a helper to build the gene pool for a given problem."
  (:require [erp12.cbgp-lite.genome :as g]
            [erp12.cbgp-lite.program.types :as t]
            [erp12.cbgp-lite.program.lib :as lib]
            [erp12.cbgp-lite.benchmark.problems.psb :as psb]
            [erp12.cbgp-lite.benchmark.problems.composite :as composite]
            [erp12.cbgp-lite.benchmark.problems.odd :refer [odd-problem]]))

(def suites
  {:psb       psb/problems
   :composite composite/problems})

(def problems
  (assoc (reduce merge (vals suites))
         "odd" odd-problem))

(defn genes-for-problem
  "Returns a sequence of genes to put into the genetic source for the given problem info."
  [{:keys [input-symbols type-ctors extra-genes]}]
  (let [input-genes  (map #(g/->Var %) input-symbols)
        ;; Ensure that functions of a all arities are included. 
        type-ctors   (->> (range 5)
                          (map t/fn-ctor)
                          (reduce conj type-ctors)
                          (set))
        lib-vars     (->> lib/type-env
                          ;; Only include Vars that use type constructors relevant to the problem.
                          (filter (fn [[_ scheme]]
                                    (every? #(contains? type-ctors %)
                                            (t/type-ctors scheme))))
                          (map #(g/->Var (first %))))
        ;; These genes should always be present.
        always-genes [(g/->Local nil) (g/->App) (g/->Let) :close]]
    (concat input-genes
            extra-genes
            lib-vars
            always-genes)))
