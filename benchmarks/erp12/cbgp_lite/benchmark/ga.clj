(ns erp12.cbgp-lite.benchmark.ga
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.search.individual :as i]
            [erp12.cbgp-lite.search.plushy :as pl]
            [erp12.cbgp-lite.task :as task]
            [erp12.ga-clj.search.ga :as ga]
            [erp12.ga-clj.toolbox :as tb]
            [taoensso.timbre :as log]
            [taoensso.timbre.appenders.core :as log-app]))

(log/merge-config!
 {:output-fn (partial log/default-output-fn {:stacktrace-fonts {}})
  :appenders {:println (assoc (log-app/println-appender) :min-level :info)}})

(def default-config
  {:n-train              200
   :n-test               2000
   :population-size      1000
   :max-generations      300
   :umad-rate            0.1
   :min-genome-size      50
   :max-genome-size      250
   :penalty              1e6
   :simplification-steps 2000
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Experimental
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Supported -  nil, :biggest, :newest, or a function from state to unboxed AST.
   ;; `nil` will search the stack for the top AST of a valid type.
   :state-output-fn      nil})

(defn make-breed
  [opts]
  (let [select (tb/make-lexicase-selection opts)
        mutate (tb/make-size-neutral-umad (assoc opts :rate (:umad-rate opts)))]
    (fn breed [state]
      (-> state
          ;; Take 1 individual per error vector.
          (->> :grouped vals (map rand-nth))
          ;; Select a parent and mutate to child
          (select state)
          :genome
          mutate))))

(defn run
  [{:keys [type-counts-file] :as opts}]
  (log/info "Options:"
            (->> opts
                 (map (fn [[k v]] (str (pr-str k) "\t" (pr-str v))))
                 (str/join "\n")
                 (str "\n")))
  (when type-counts-file
    (log/warn "Type counting enabled. This is slow!")
    (reset! c/collect-types? true)
    (reset! c/types-seen {}))
  (let [config (merge default-config opts)
        task (-> config
                 bu/read-problem
                 task/enhance-task
                 (assoc :evaluate-fn i/evaluate-full-behavior))
        opts (merge config task)
        _ (log/info "Type Constructors: " (:type-ctors opts))
        _ (log/info "Vars:" (:vars opts))
        evaluator (i/make-evaluator (-> opts
                                        (assoc :cases (:train task))
                                        (dissoc :train :test)))
        {:keys [best result]} (ga/run {:population-size (:population-size config)
                                       :genome-factory  #(pl/random-plushy-genome opts)
                                       :pre-eval        (let [{:keys [downsample-rate train]} opts]
                                                          (fn [{:keys [step]}]
                                                            (log/info "STARTING" step)
                                                            {:cases      (if downsample-rate
                                                                           (random-sample downsample-rate train)
                                                                           train)
                                                             :step-start (System/currentTimeMillis)}))
                                       :evaluator       evaluator
                                       :post-eval       (fn [{:keys [individuals]}]
                                                          (doseq [[stat-name stat-val]
                                                                  (sort-by key
                                                                           (bu/aggregate-stats {:code-depth            bu/code-depth-stat
                                                                                                :code-depth-over-size  bu/code-depth-over-size-stat
                                                                                                :code-size             bu/code-size-stat
                                                                                                :exceptions            bu/exception-messages-stat
                                                                                                :genome-size           bu/genome-size-stat
                                                                                                :lowest-error-per-case bu/lowest-error-per-case
                                                                                                :num-no-ast            bu/num-no-ast-stat
                                                                                                :num-penalties         (bu/make-num-penalty-stat (:penalty opts))
                                                                                                :num-throwing          bu/num-throwing-stat
                                                                                                :total-error           bu/total-error-stat
                                                                                                :unique-behaviors      bu/unique-behaviors-stat}
                                                                                               individuals))]
                                                            (log/info stat-name stat-val))
                                                          {:grouped (group-by :errors individuals)})
                                       :breed           (make-breed opts)
                                       :individual-cmp  (comparator #(< (:total-error %1) (:total-error %2)))
                                       :stop-fn         (let [{:keys [max-generations cases]} opts]
                                                          (fn [{:keys [step step-start best new-best?]}]
                                                            (log/info :best-individual-errors (:errors best))
                                                            (log/info "REPORT"
                                                                      {:step       step
                                                                       :duration   (- (System/currentTimeMillis) step-start)
                                                                       :best-error (:total-error best)
                                                                       :best-code  (:code best)})
                                                            (cond
                                                              (= step max-generations) :max-generation-reached
                                                              ;; If the "best" individual has solved the subset of cases
                                                              ;; Test if on the full training set.
                                                              (zero? (:total-error best))
                                                              (if (and new-best? (zero? (:total-error (evaluator (:genome best) {:cases cases}))))
                                                                :solution-found
                                                                ;; If an individual solves a batch but not all training cases,
                                                                ;; no individual can become the new best and the run will fail.
                                                                ;; @todo Fix this in ga-clj somehow?
                                                                (log/info "Best individual solved a batch but not all training cases.")))))
                                       :mapper          pmap})
        _ (log/info "PRE-SIMPLIFICATION" best)
        ;; Simplify the best individual seen during evolution.
        best (i/simplify {:individual           best
                          :simplification-steps (:simplification-steps config)
                          :evaluator            evaluator})
        _ (log/info "POST-SIMPLIFICATION" best)
        ;; Evaluate the final program on the unseen test cases.
        {:keys [solution?]} (i/evaluate-full-behavior {:func     (:func best)
                                                       :cases    (:test task)
                                                       :loss-fns (:loss-fns task)
                                                       :penalty  (:penalty default-config)})]
    (log/info "BEST CODE" (let [code (:code best)]
                            (if (coll? code)
                              (reverse (into '() code))
                              code)))

    (if (= :solution-found result)
      (do
        (log/info "SOLUTION FOUND")
        (if solution?
          (log/info "SOLUTION GENERALIZED")
          (log/info "SOLUTION FAILED TO GENERALIZE")))
      (log/info "SOLUTION NOT FOUND"))

    (when type-counts-file
      (log/info "Writing type frequencies to" type-counts-file)
      (with-open [w (io/writer type-counts-file :append true)]
        (.write w "[")
        (doseq [[typ freq] @c/types-seen]
          (.write w (prn-str {:type typ :freq freq})))
        (.write w "]")))

    (:func best)))

(comment
  (run {:suite-ns        'erp12.cbgp-lite.benchmark.suite.psb
        :data-dir        "data/psb/"
        :problem         "vectors-summed"}))