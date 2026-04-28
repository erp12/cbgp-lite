(ns erp12.cbgp-lite.benchmark.research.lineage
  "Research analysis of genome lineage and ancestry across saved population snapshots."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.walk :as w]
            [erp12.cbgp.benchmark.problems :refer [problems genes-for-problem]]
            [erp12.cbgp-lite.genome :as g]
            [erp12.cbgp-lite.individual :as i]
            [erp12.cbgp-lite.program.expr :as e]
            [erp12.cbgp-lite.program.types :as t]
            [erp12.ga-clj.toolbox :as tb]))

(def SNAPSHOT-PATH-TEMPLATE "data/snapshots/experiment=locality/problem=%s/trial=%s/gen=%s/genomes.edn")
(def SAMPLE-SIZE 10)
(def STEPS 1200)

(def ids (atom {}))
(defn get-or-create-id
  [x]
  (if-let [entry (find @ids x)]
    (val entry)
    (let [new-id (gensym "p")]
      (swap! ids assoc x new-id)
      new-id)))

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

(defn remove-lit-vals
  [de-bruijn-expr]
  (w/postwalk
   (fn [{:keys [kind] :as e}]
     (if (= :Lit kind)
       {:kind :Lit}
       e))
   de-bruijn-expr))

(defn lineage-factory
  [{:keys [problem umad-rate]}]
  (let [problem-info   (get problems problem)
        mutate         (tb/make-size-neutral-umad
                        {:rate           umad-rate
                         :genetic-source (g/make-genetic-source (genes-for-problem problem-info))})
        evaluator (i/make-genome-evaluator (assoc problem-info :cases [] :loss-fns []))
        genome->code (fn just-compile [genome]
                       (let [compiled (evaluator genome {})]
                         {:code_id (when (:expr compiled)
                                     (get-or-create-id (remove-lit-vals (e/de-bruijn (:expr compiled)))))
                          :code (:code compiled)}))]
    (fn [lineage-id genome]
      (let [{:keys [code code_id]} (genome->code genome)]
        (loop [step    1
               gn      (mutate genome)
               results [{:lineage lineage-id
                         :step 0
                         :code (pr-str code)
                         :code_id code_id}]]
          (if (= step STEPS)
            results
            (recur (inc step)
                   (mutate gn)
                   (let [{:keys [code code_id]} (genome->code gn)]
                     (conj results
                           {:lineage lineage-id
                            :step step
                            :code (pr-str code)
                            :code_id code_id})))))))))

(defn generate-datasets
  [spec]
  (doseq [[problem trial generation] spec]
    (let [filepath (format SNAPSHOT-PATH-TEMPLATE problem trial generation)
          generate-lineage (lineage-factory {:problem   problem
                                             :umad-rate 0.1})]
      (println filepath)
      (if (.exists (io/file filepath))
        (let [dataset     (->> (read-snapshot filepath)
                               (take SAMPLE-SIZE)
                               (map generate-lineage (range SAMPLE-SIZE))
                               (reduce concat))
              output-file (io/file (format "data/lineage/problem=%s/lineage.jsonl"
                                           problem))]
          (io/make-parents output-file)
          (with-open [writer (io/writer output-file)]
            (doseq [line dataset]
              (.write writer (json/write-str line))
              (.write writer "\n"))))
        (println "Skipped. Doesn't exist.")))))
         

(comment
  
  (generate-datasets [;; Problem, trial, generation
                      ["number-io" 0 0]
                      ["vectors-summed" 3 50]
                      ["count-odds" 0 250]
                      ["fuel-cost" 0 200]
                      ["compare-string-lengths" 0 200]])
  
  *e

  @ids

  (reset! ids {})

  (comment))
  
  