(ns erp12.cbgp-lite.benchmark.research.stack-shape
  "Research analysis of compiler stack shape and utilization across saved population snapshots."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [erp12.cbgp-lite.benchmark.problems :refer [problems]]
            [erp12.cbgp-lite.genome :as g]
            [erp12.cbgp-lite.program.compile :refer [push->ast]]
            [erp12.cbgp-lite.program.lib :as lib]
            [erp12.cbgp-lite.program.types :as t]))

(def SNAPSHOT-PATH-TEMPLATE "data/snapshots/experiment=large-genomes/problem=%s/trial=%s/gen=%s/genomes.edn")
(def GENERATIONS [0 50 100 150 200 250 300])
(def TRIALS [0])
(def SAMPLE-SIZE 100)
(def COMPILATION-REPORT-INTERVAL 10)

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

(defn compilation-tracer
  [{:keys [problem trial gen]}]
  (let [problem-info     (get problems problem)
        input-schemes    (mapv #(t/generalize % lib/type-env) (:input-types problem-info))
        type-env         (into lib/type-env (zipmap (:input-symbols problem-info) input-schemes))
        stack-depth-file (io/file (format "data/stack-depth/problem=%s/stack-depth.jsonl" problem))
        ast-sizes-file   (io/file (format "data/stack-ast-sizes/problem=%s/stack-ast-sizes.jsonl" problem))
        step-hook        (fn step-hook [{:keys [step ast-sizes]}]
                           (when (zero? (mod step COMPILATION-REPORT-INTERVAL))
                             (with-open [writer (io/writer stack-depth-file :append true)]
                               (.write writer (json/write-str {:problem problem
                                                               :trial trial
                                                               :gen gen
                                                               :step step
                                                               :stack_depth (count ast-sizes)}))
                               (.write writer "\n"))
                             (with-open [writer (io/writer ast-sizes-file :append true)]
                               (doseq [[idx ast-size] (map-indexed vector ast-sizes)]
                                 (.write writer (json/write-str {:problem problem
                                                                 :trial trial
                                                                 :gen gen
                                                                 :step step
                                                                 :stack_index idx
                                                                 :ast_size ast-size}))
                                 (.write writer "\n")))))]
    (io/make-parents stack-depth-file)
    (io/make-parents ast-sizes-file)
    (fn trace-genome-compilation
      [genome]
      (push->ast {:push (g/plushy->push genome)
                  :output-type (:output-type problem-info)
                  :type-env type-env
                  :hooks {:step step-hook}}))))

(defn generate-datasets
  [problems]
  (doseq [problem problems
          trial TRIALS
          generation GENERATIONS]
    (let [filepath (format SNAPSHOT-PATH-TEMPLATE problem trial generation)]
      (println "Problem" problem "\tTrial" trial "\tGeneration" generation "\tInput" filepath)
      (if (.exists (io/file filepath))
        (let [genomes  (take SAMPLE-SIZE (read-snapshot filepath))
              tracer (compilation-tracer {:problem problem, :trial trial, :gen generation})]
          (doseq [genome genomes]
            (tracer genome)))
        (println "Skipped. Doesn't exist.")))))
         

(comment
  
  (generate-datasets ["vectors-summed"
                      "count-odds"
                      "fuel-cost" 
                      "number-io" 
                      "compare-string-lengths"])

  *e

  (comment))
  