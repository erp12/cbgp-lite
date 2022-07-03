(ns erp12.cbgp-lite.benchmark.ga
  (:require [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.search.individual :as i]
            [erp12.cbgp-lite.search.pluhsy :as pl]
            [erp12.cbgp-lite.task :as task]
            [erp12.ga-clj.search.ga :as ga]
            [erp12.ga-clj.toolbox :as tb]
            [taoensso.timbre :as log]
            [taoensso.timbre.appenders.core :as log-app]))

(log/merge-config!
  {:output-fn (partial log/default-output-fn {:stacktrace-fonts {}})
   :appenders {:println (assoc (log-app/println-appender) :min-level :info)
               ;:spit    (assoc (log-app/spit-appender {:fname "./errors.log"}) :min-level :debug)
               }})

(defn make-breed
  [{:keys [umad-rate] :as opts}]
  (let [select (tb/make-lexicase-selection opts)
        mutate (tb/make-size-neutral-umad {:rate           umad-rate
                                           :genetic-source (:genetic-source opts)})]
    (fn [generation]
      (let [to-mutate (->> generation select :genome)]
        (mutate to-mutate)))))

(def default-config
  {:n-train              100
   :n-test               300
   :population-size      500                                ; 1000
   :max-generations      100                                ; 300
   :umad-rate            0.1
   :min-genome-size      50
   :max-genome-size      250
   :penalty              1e5
   :simplification-steps 2000})

(defn run
  [opts]
  ;(doseq [[k v] opts]
  ;  (println k ":" (class k) " -> " v ":" (class v)))
  (let [config (merge default-config opts)
        task (-> opts
                 (assoc :config config)
                 bu/read-problem
                 task/enhance-task
                 (assoc :evaluate-fn i/evaluate-full-behavior))
        opts (merge config task)
        individual-factory (i/make-individual-factory (-> opts
                                                          (assoc :cases (:train task))
                                                          (dissoc :train :test)))
        {:keys [best result]} (ga/run {;; The number of genomes to include in each generation.
                                       :population-size    (:population-size config)
                                       ;; Function for generating random genomes.
                                       :genome-factory     #(pl/random-plushy-genome opts)
                                       ;; An initialization function for each generation.
                                       ;; Optionally, will select a subset of training cases to use as the downsample.
                                       :pre-generation     (let [{:keys [downsample-rate cases]} opts]
                                                             (fn [{:keys [step]}]
                                                               (log/info "STARTING STEP" step)
                                                               {:cases (if downsample-rate
                                                                         (random-sample downsample-rate cases)
                                                                         cases)}))
                                       ;; Function for converting genomes into compiled Clojure functions
                                       ;; and associated metadata for tracking progress (number of evaluations).
                                       :individual-factory individual-factory
                                       ;; A function for breeding child genomes from a population of individuals.
                                       :breed              (make-breed opts)
                                       ;; A comparator function between individuals for selecting the best
                                       ;; seen individual in the run so far.
                                       :individual-cmp     (comparator #(< (:total-error %1) (:total-error %2)))
                                       ;; A predicate function for determining when evolution should stop.
                                       :stop-fn            (let [{:keys [max-generations cases]} opts]
                                                             (fn [{:keys [step best new-best?]}]
                                                               (log/info "REPORT"
                                                                         {:step       step
                                                                          :best-error (:total-error best)
                                                                          :best-code  (:code best)})
                                                               (cond
                                                                 (= step max-generations) :max-generation-reached
                                                                 ;; If the "best" individual has solved the subset of cases
                                                                 ;; Test if on the full training set.
                                                                 (zero? (:total-error best))
                                                                 (if (and new-best? (zero? (:total-error (individual-factory (:genome best) {:cases cases}))))
                                                                   :solution-found
                                                                   ;; If an individual solves a batch but not all training cases,
                                                                   ;; no individual can become the new best and the run will fail.
                                                                   ;; @todo Fix this in ga-clj somehow?
                                                                   (log/info "Best individual solved a batch but not all training cases.")))))
                                       :mapper             pmap})
        ;; Simplify the best individual seen during evolution.
        best (i/simplify {:individual           best
                          :simplification-steps (:simplification-steps config)
                          :individual-factory   individual-factory})
        ;; Evaluate the final program on the unseen test cases.
        {:keys [solution?]} (i/evaluate-full-behavior {:code        (:code best)
                                                       :arg-symbols (:arg-symbols task)
                                                       :cases       (:test task)
                                                       :loss-fns    (:loss-fns task)
                                                       :penalty     (:penalty default-config)})]
    (log/info "BEST INDIVIDUAL" best)
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
    (:func best)))

(comment

  (run {:suite-ns 'erp12.cbgp-lite.benchmark.suite.psb
        :data-dir "data/psb/"
        :problem  "replace-space-with-newline"})

  )