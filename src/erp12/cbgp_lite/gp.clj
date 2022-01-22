(ns erp12.cbgp-lite.gp
  (:require [taoensso.timbre :as log]
            [taoensso.timbre.appenders.core :as log-app]
            [erp12.ga-clj.generational :refer [evolve]]
            [erp12.cbgp-lite.gp.pluhsy :as pl]
            [erp12.cbgp-lite.gp.individual :as indiv]
            [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.lang.lib :as lib]))

(log/merge-config!
  {:output-fn (partial log/default-output-fn {:stacktrace-fonts {}})
   :appenders {:println (assoc (log-app/println-appender) :min-level :info)
               :spit    (assoc (log-app/spit-appender {:fname "./errors.log"}) :min-level :debug)}})

(defn make-individual-factory
  [{:keys [input->type return-type vars loss-fn penalty]}]
  (let [arg-symbols (vec (keys input->type))
        type-environment (->> lib/library
                              (filter (fn [[symb _]] (contains? vars symb)))
                              (merge input->type)
                              (mapv (fn [[symb typ]] [:= symb typ])))]
    (fn [gn {:keys [cases]}]
      (let [;; Get Push code from the genome.
            push (pl/plushy->push gn)
            ;; Compile the Push into a Clojure form that accepts and returns the
            ;; correct types.
            code (c/push->clj {:push      push
                               :inputs    arg-symbols
                               :ret-type  return-type
                               :type-env  type-environment
                               :dealiases lib/dealiases})
            ;; Create a Clojure function with the compiled code.
            func (c/synth-fn arg-symbols code)
            ;; Call the function on each training case.
            behavior (->> cases
                          (map :inputs)
                          (map #(try
                                  (apply func %)
                                  (catch Exception e e))))
            ;; Compute the error on each case.
            errors (do
                     (when-let [ex (first (filter #(instance? Exception %) behavior))]
                       (log/debug {:ex   (class ex)
                                   :msg  (.getMessage ex)
                                   :code code}))
                     (->> cases
                          (map :output)
                          (mapv #(if (or (nil? %1) (instance? Exception %1))
                                   penalty
                                   (loss-fn %1 %2))
                                behavior)))]
        {:push        push
         :code        code
         :func        func
         :behavior    behavior
         :errors      errors
         :total-error (apply + errors)}))))

(defn run
  [{:keys [max-generations simplification-steps cases downsample-rate] :as opts}]
  (let [individual-factory (make-individual-factory opts)
        evo-result (evolve
                     (merge {:genome-factory     #(pl/random-plushy-genome opts)
                             :pre-generation     (fn [{:keys [step]}]
                                                   (log/info "STARTING STEP" step)
                                                   {:cases (random-sample downsample-rate cases)})
                             :individual-factory individual-factory
                             :individual-cmp     (comparator #(< (:total-error %1) (:total-error %2)))
                             :stop-fn            (fn [{:keys [step best]}]
                                                   (log/info "REPORT"
                                                             {:step       step
                                                              :best-error (:total-error best)
                                                              :best-code  (:code best)})
                                                   (cond
                                                     (= step max-generations) :max-generation-reached

                                                     ;; If the "best" individual has solved the subset of cases
                                                     ;; Test if on the full training set.
                                                     (zero? (:total-error best))
                                                     (if (zero? (:total-error (individual-factory (:genome best) {:cases cases})))
                                                       :solution-found
                                                       (log/info "Best individual solved a batch but not all training cases."))))
                             :mapper             pmap}
                            opts))
        _ (log/info "RESULT" (:result evo-result) "AT" (:step evo-result))
        best (:best evo-result)
        _ (log/info "PRE-SIMPLIFICATION" best)
        simplified (indiv/simplify {:individual           best
                                    :simplification-steps simplification-steps
                                    :individual-factory   individual-factory
                                    :context              {:cases cases}})]
    (log/info "POST-SIMPLIFICATION" simplified)
    (assoc evo-result :best simplified)))
