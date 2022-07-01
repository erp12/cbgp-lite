(ns erp12.cbgp-lite.gp
  (:require [clj-fuzzy.levenshtein :as lev]
            [erp12.cbgp-lite.gp.individual :as indiv]
            [erp12.cbgp-lite.gp.pluhsy :as pl]
            [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.ga-clj.generational :refer [evolve]]
            [taoensso.timbre :as log]
            [taoensso.timbre.appenders.core :as log-app])
  (:import (java.io StringWriter)))

(log/merge-config!
  {:output-fn (partial log/default-output-fn {:stacktrace-fonts {}})
   :appenders {:println (assoc (log-app/println-appender) :min-level :info)
               ;:spit    (assoc (log-app/spit-appender {:fname "./errors.log"}) :min-level :debug)
               }})

(defmacro with-out-and-stdout
  [form]
  `(with-open [s# (StringWriter.)]
     (binding [*out* s#]
       {:output  ~form
        :std-out (str s#)})))

(defn evaluate-code
  [{:keys [code arg-symbols cases loss-fns penalty]}]
  (let [;; Create a Clojure function with the compiled code.
        func (c/synth-fn arg-symbols code)
        ;; Call the function on each training case.
        behavior (->> cases
                      (map :inputs)
                      (map #(try
                              (with-out-and-stdout (apply func %))
                              (catch Exception e
                                {:output e :std-out nil}))))
        ;; Compute the error on each case.
        errors (do
                 ;; Debug log any errors thrown during program evaluation to help in
                 ;; debugging the library of functions and their type annotations.
                 (when-let [ex (first (filter #(instance? Exception %) behavior))]
                   (log/debug {:ex   (class ex)
                               :msg  (.getMessage ex)
                               :code code}))
                 (->> cases
                      (mapcat (fn [actual expected]
                                (conj
                                  ;; For each loss function, compute the loss or give penalty.
                                  (mapv #(if (or (nil? (:output actual))
                                                 (instance? Exception (:output actual)))
                                           penalty
                                           (% (:output actual) (:output expected)))
                                        loss-fns)
                                  ;; Append std-out error, when expected.
                                  (when (contains? expected :std-out)
                                    (if (nil? (:std-out actual))
                                      penalty
                                      (lev/distance (:std-out actual) (:std-out expected))))))
                              behavior)
                      ;; When there is no std-out error, filter out the nils.
                      (filter some?)
                      vec))]
    {:func        func
     :behavior    behavior
     :errors      errors
     :total-error (apply + errors)}))

(defn make-individual-factory
  [{:keys [input->type return-type vars loss-fns penalty]}]
  (let [arg-symbols (vec (sort (keys input->type)))
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
                               :dealiases lib/dealiases})]
        (merge (evaluate-code {:code        code
                               :arg-symbols arg-symbols
                               :cases       cases
                               :loss-fns    loss-fns
                               :penalty     penalty})
               {:push push
                :code code})))))

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
                             :stop-fn            (fn [{:keys [step best new-best?]}]
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
