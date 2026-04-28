(ns erp12.cbgp-lite.benchmark.research.locality
  "Research analysis of genetic locality: measures how single-step mutations change program behavior across population snapshots."
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clj-fuzzy.levenshtein :as lev]
            [erp12.ga-clj.toolbox :as tb]
            [erp12.cbgp-lite.genome :as g]
            [erp12.cbgp-lite.program.types :as t]
            [erp12.cbgp-lite.program.expr :as e]
            [erp12.cbgp-lite.individual :as i]
            [erp12.cbgp-lite.benchmark.problems :refer [problems genes-for-problem]]))

(def SNAPSHOT-PATH-TEMPLATE "data/snapshots/experiment=locality/problem=%s/trial=%s/gen=%s/genomes.edn")
(def GENERATIONS [0 50 100 150 200 250 300])
(def TRIALS [0 1 2 3 4])
(def SAMPLE-SIZE 30)
(def GRAPH-SIZE 20)

(defn read-snapshot
  [path]
  (with-open [r (io/reader path)]
    (edn/read {:readers {'erp12.cbgp-lite.program.types.TypeVar         t/map->TypeVar
                         'erp12.cbgp-lite.program.types.TypeConstructor t/map->TypeConstructor
                         'erp12.cbgp-lite.program.types.TypeApp         t/map->TypeApp
                         'erp12.cbgp-lite.genome.Var                    g/map->Var
                         'erp12.cbgp-lite.genome.Local                  g/map->Local
                         'erp12.cbgp-lite.genome.Lit                    g/map->Lit
                         'erp12.cbgp-lite.genome.App                    g/map->App
                         'erp12.cbgp-lite.genome.Abs                    g/map->Abs
                         'erp12.cbgp-lite.genome.Let                    g/map->Let}}
              (java.io.PushbackReader. r))))


(defn neighbor-graph-factory
  [{:keys [problem mutate]}]
  (let [problem-info   (get problems problem)
        read-cases     (:dataset-reader problem-info)
        cases          (:train (read-cases (merge problem-info
                                                  {:data-dir "./data/psb"
                                                   :problem problem
                                                   :n-train 100
                                                   :n-test 0})))
        evaluate       (i/make-genome-evaluator (assoc problem-info :cases cases))]
    (fn neighbor-graph [genome]
      (let [evaluation     (evaluate genome {})
            expr           (:expr evaluation)
            de-bruijn-tree (when expr (e/de-bruijn expr))]
        (repeatedly GRAPH-SIZE
                    #(let [neighbor-genome     (mutate genome)
                           neighbor-evaluation (evaluate neighbor-genome {})
                           neighbor-expr       (:expr neighbor-evaluation)
                           neighbor-de-bruijn  (when neighbor-expr (e/de-bruijn neighbor-expr))]
                       {:individual         (when expr (pr-str (:code evaluation)))
                        :neighbor           (when neighbor-expr (pr-str (:code neighbor-evaluation)))
                        :genotype_distance  (lev/distance genome neighbor-genome)
                        :phenotype_distance (e/tree-edit-distance de-bruijn-tree neighbor-de-bruijn)
                        :behavior_distance  (count (filter true? (map not= (:behavior evaluation) (:behavior neighbor-evaluation))))}))))))

(defn generate-datasets
  [{:keys [problem umad-rate]}]
  (let [problem-info     (get problems problem)
        mutate           (tb/make-size-neutral-umad {:rate           umad-rate
                                                     :genetic-source (g/make-genetic-source (genes-for-problem problem-info))})
        ->neighbor-graph (neighbor-graph-factory {:problem problem
                                                  :mutate  mutate})
        sub-graphs       (for [trial      TRIALS
                               generation GENERATIONS]
                           (let [filepath (format SNAPSHOT-PATH-TEMPLATE problem trial generation)]
                             (println "Trial" trial "\tGeneration" generation "\tInput" filepath)
                             (if (.exists (io/file filepath))
                               (->> (read-snapshot filepath)
                                    (take SAMPLE-SIZE)
                                    (mapcat #(->neighbor-graph %))
                                    (map #(assoc % :trial trial :generation generation)))
                               (println "Skipped. Doesn't exist."))))
        full-graph       (mapcat identity sub-graphs)
        output-file      (io/file (format "data/locality/problem=%s/umad=%s/locality.jsonl"
                                          problem umad-rate))]
    (io/make-parents output-file)
    (with-open [writer (io/writer output-file)]
      (doseq [line full-graph]
        (.write writer (json/write-str line))
        (.write writer "\n")))))

(comment
  
  (doseq [problem   ["number-io" "vectors-summed" "count-odds" "fuel-cost" "compare-string-lengths"]
          umad-rate [0.1 0.2]]
    (println problem umad-rate)
    (generate-datasets {:problem   problem
                        :umad-rate umad-rate}))

  *e

  (println "\n")

  (comment))
  