(ns erp12.cbgp-lite.benchmark.hill-climbing
  (:require [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.search.individual :as i]
            [erp12.cbgp-lite.search.plushy :as pl]
            [erp12.cbgp-lite.task :as task]
            [erp12.ga-clj.search.hill-climbing :as hc]
            [erp12.ga-clj.toolbox :as tb]
            [taoensso.timbre :as log]
            [taoensso.timbre.appenders.core :as log-app]))

(log/merge-config!
  {:output-fn (partial log/default-output-fn {:stacktrace-fonts {}})
   :appenders {:println (assoc (log-app/println-appender) :min-level :info)
               ;:spit    (assoc (log-app/spit-appender {:fname "./errors.log"}) :min-level :debug)
               }})

(def default-config
  {:n-train              100
   :n-test               300
   ;; Comparable to GA: population size * max generations
   :max-steps            (* 500 100)
   :steps-before-restart 250
   :umad-rate            0.1
   :min-genome-size      50
   :max-genome-size      250
   :penalty              1e5
   :simplification-steps 2000
   :report-period        100})

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
        individual-factory (i/make-evaluator (-> opts
                                                 (assoc :cases (:train task))
                                                 (dissoc :train :test)))
        {:keys [individual result]} (hc/run {;; Function for generating random genomes.
                                             :genome-factory       #(pl/random-plushy-genome opts)
                                             ;; Function for converting genomes into compiled Clojure functions
                                             ;; and associated metadata for tracking progress (number of evaluations).
                                             :individual-factory   individual-factory
                                             ;;
                                             :mutate               (let [umad (tb/make-size-neutral-umad
                                                                                {:rate           (:umad-rate opts)
                                                                                 :genetic-source (:genetic-source opts)})]
                                                                     #(umad (:genome %)))
                                             ;; We compare individuals on the basis of the total error.
                                             :individual-cmp       (comparator #(< (:total-error %1) (:total-error %2)))
                                             ;; Predicate function for determining when search should stop.
                                             :stop-fn              (let [{:keys [report-period max-steps]} opts]
                                                                     (fn [{:keys [step individual]}]
                                                                       (when (zero? (mod step report-period))
                                                                         (log/info "REPORT"
                                                                                   {:step       step
                                                                                    :best-error (:total-error individual)
                                                                                    :best-code  (:code individual)}))
                                                                       (cond
                                                                         (= (:total-error individual) 0) :solution-found
                                                                         (>= step max-steps) :max-step-reached)))
                                             :max-steps            (:max-steps opts)
                                             :steps-before-restart (:steps-before-restart opts)})
        ;; Simplify the best individual seen during evolution.
        best (i/simplify {:individual           individual
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
