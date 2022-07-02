(ns erp12.cbgp-lite.benchmark.random-search
  (:require [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.search.individual :as i]
            [erp12.cbgp-lite.search.pluhsy :as pl]
            [erp12.cbgp-lite.task :as task]
            [erp12.ga-clj.search.random :as rs]
            [taoensso.timbre :as log]))

(def default-config
  {:n-train              100
   :n-test               300
   ;; Comparable to GA: population size * max generations * training cases
   :max-evaluations      (* 1000 300 100)
   :min-genome-size      50
   :max-genome-size      250
   :simplification-steps 2000
   :report-period        100})

(defn run
  [opts]
  ;(doseq [[k v] opts]
  ;  (println k ":" (class k) " -> " v ":" (class v)))
  (let [config (merge default-config opts)
        task (-> opts
                 (assoc :config config)
                 bu/read-problem
                 task/enhance-task
                 (assoc :evaluate-fn i/evaluate-until-first-failure))
        opts (merge config task)
        individual-factory (i/make-individual-factory (-> opts
                                                          (assoc :cases (:train task))
                                                          (dissoc :train :test)))
        {:keys [individual result]} (rs/run {;; Function for generating random genomes.
                                             :genome-factory     #(pl/random-plushy-genome opts)
                                             ;; Function for converting genomes into compiled Clojure functions
                                             ;; and associated metadata for tracking progress (number of evaluations).
                                             :individual-factory individual-factory
                                             ;; Predicate function for determining when search should stop.
                                             :stop-fn            (let [{:keys [report-period max-evaluations]} opts
                                                                       eval-counter (volatile! 0)]
                                                                   (fn [{:keys [step individual]}]
                                                                     (when (zero? (mod step report-period))
                                                                       (log/info "REPORT"
                                                                                 {:step        step
                                                                                  :evaluations @eval-counter}))
                                                                     (let [{:keys [cases-used solution?]} individual]
                                                                       (vswap! eval-counter + cases-used)
                                                                       (cond
                                                                         solution? :solution-found
                                                                         (> @eval-counter max-evaluations) :max-evaluations-reached))))})
        ;; Evaluate the final program on the unseen test cases.
        {:keys [solution?]} (i/evaluate-until-first-failure {:code        (:code individual)
                                                             :arg-symbols (:arg-symbols task)
                                                             :cases       (:test task)
                                                             :loss-fns    (:loss-fns task)})]
    (if (= :solution-found result)
      (do
        (log/info "SOLUTION FOUND")
        (if solution?
          (log/info "SOLUTION GENERALIZED")
          (log/info "SOLUTION FAILED TO GENERALIZE")))
      (log/info "SOLUTION NOT FOUND"))
    (:func individual)))

(comment

  (run {:suite-ns 'erp12.cbgp-lite.benchmark.suite.psb
        :data-dir "data/psb/"
        :problem  "number-io"})

  )