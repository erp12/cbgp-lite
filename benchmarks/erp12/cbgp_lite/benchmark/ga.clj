(ns erp12.cbgp-lite.benchmark.ga
  (:require [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.compile]
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

(def default-config
  {:n-train              200
   :n-test               2000
   :population-size      1000
   :max-generations      300
   :umad-rate            0.1
   :min-genome-size      50
   :max-genome-size      250
   :penalty              1e5
   :simplification-steps 2000})

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
  [opts]
  ;(doseq [[k v] opts]
  ;  (println k ":" (class k) " -> " v ":" (class v)))
  (let [config (merge default-config opts)
        task (-> config
                 bu/read-problem
                 task/enhance-task
                 (assoc :evaluate-fn i/evaluate-full-behavior))
        opts (merge config task)
        ;; @todo Refactor "individual-factory" to "genome->individual"
        individual-factory (i/make-evaluator (-> opts
                                                 (assoc :cases (:train task))
                                                 (dissoc :train :test)))
        {:keys [best result]} (ga/run {:population-size (:population-size config)
                                       :genome-factory  #(pl/random-plushy-genome opts)
                                       :pre-eval        (let [{:keys [downsample-rate train]} opts]
                                                          (fn [{:keys [step]}]
                                                            (log/info "Starting step" step)
                                                            {:cases      (if downsample-rate
                                                                           (random-sample downsample-rate train)
                                                                           train)
                                                             :step-start (System/currentTimeMillis)}))
                                       :evaluator      individual-factory
                                       :post-eval       (fn [{:keys [individuals]}]
                                                          {:grouped (group-by :errors individuals)})
                                       :breed           (make-breed opts)
                                       :individual-cmp  (comparator #(< (:total-error %1) (:total-error %2)))
                                       :stop-fn         (let [{:keys [max-generations cases]} opts]
                                                          (fn [{:keys [step step-start best new-best?]}]
                                                            (log/info "Report"
                                                                      {:step       step
                                                                       :duration   (- (System/currentTimeMillis) step-start)
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
                                       :mapper          pmap
                                       })
        ;; Simplify the best individual seen during evolution.
        best (i/simplify {:individual           best
                          :simplification-steps (:simplification-steps config)
                          :individual-factory   individual-factory})
        ;; Evaluate the final program on the unseen test cases.
        {:keys [solution?]} (i/evaluate-full-behavior {:func     (:func best)
                                                       :cases    (:test task)
                                                       :loss-fns (:loss-fns task)
                                                       :penalty  (:penalty default-config)})]
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
