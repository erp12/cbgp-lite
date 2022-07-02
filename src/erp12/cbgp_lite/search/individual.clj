(ns erp12.cbgp-lite.search.individual
  (:require [clj-fuzzy.levenshtein :as lev]
            [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.search.pluhsy :as pl]
            [taoensso.timbre :as log])
  (:import (java.io StringWriter)))

(defmacro with-out-and-stdout
  [form]
  `(let [s# (new StringWriter)]
     (binding [*out* s#]
       ;; Let binding is required to force evaluation of the form to happen
       ;; before the StringWriter is converted to the std-out string.
       (let [result# ~form]
         {:output  result#
          :std-out (str s#)}))))


(defn log-program-execution-errors
  "Debug log any errors thrown during program evaluation to help with
  debugging the library of functions and their type annotations."
  [{:keys [code output]}]
  (when (instance? Exception output)
    (log/debug {:ex   (class output)
                :msg  (.getMessage output)
                :code code})
    true))

(defn compute-errors-on-case
  [{:keys [case penalty loss-fns prog-output]}]
  (let [{actual-output :output actual-stdout :std-out} prog-output
        {expected-output :output expected-stdout :std-out} case]
    (->> (conj
           (mapv (fn [lf]
                   (if (or (nil? actual-output) (instance? Exception actual-output))
                     penalty
                     (lf actual-output expected-output)))
                 loss-fns)
           (when (contains? case :std-out)
             (if (nil? actual-stdout)
               penalty
               (lev/distance actual-stdout expected-stdout))))
         (filter some?)
         vec)))

(defn evaluate-until-first-failure
  [{:keys [code arg-symbols cases loss-fns]}]
  (let [;; Create a Clojure function with the compiled code.
        func (c/synth-fn arg-symbols code)]
    (loop [cases cases
           cases-used 0]
      (if (empty? cases)
        {:func       func
         :solution?  true
         :cases-used cases-used}
        (let [{:keys [inputs] :as case} (first cases)
              prog-output (try
                            (with-out-and-stdout (apply func inputs))
                            (catch Exception e
                              {:output e :std-out nil}))
              _ (log-program-execution-errors (assoc prog-output :code code))
              errors (compute-errors-on-case {:case        case
                                              :penalty     1 ;; Any positive number will short-circuit evaluation.
                                              :loss-fns    loss-fns
                                              :prog-output prog-output})]
          (if (some pos? errors)
            {:func       func
             :cases-used (inc cases-used)}
            (recur (rest cases) (inc cases-used))))))))

(defn evaluate-full-behavior
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

        ;; Log the first execution error seen.
        _ (log-program-execution-errors (assoc (first (filter #(instance? Exception (:output %)) behavior))
                                          :code code))

        ;; Compute the error on each case.
        errors (->> cases
                    (mapcat (fn [b case]
                              (compute-errors-on-case {:case        case
                                                       :prog-output b
                                                       :loss-fns    loss-fns
                                                       :penalty     penalty}))
                            behavior)
                    ;; When there is no std-out error, filter out the nils.
                    (filter some?)
                    vec)
        total-error (apply + errors)]
    {:func        func
     :behavior    behavior
     :errors      errors
     :total-error total-error
     :solution?   (zero? total-error)
     :cases-used  (count cases)}))


(defn make-individual-factory
  [{:keys [evaluate-fn cases] :as opts}]
  (fn [gn context]
    ;(log/debug "Processing genome" (vec gn))
    (let [cases (or (:cases context) cases)
          ;_ (log/debug "Evaluating on" (count cases) "cases")
          ;; Get Push code from the genome.
          push (pl/plushy->push gn)
          ;_ (log/debug "Compiling push" push)
          ;; Compile the Push into a Clojure form that accepts and returns the
          ;; correct types.
          code (c/push->clj (assoc opts :push push))]
      ;(log/debug "Evaluating code" code)
      (merge (evaluate-fn (assoc opts
                            :code code
                            :cases cases))
             {:push push
              :code code}))))


(defn simplify
  [{:keys [individual simplification-steps individual-factory context]}]
  (log/info "PRE-SIMPLIFICATION" individual)
  (let [simplified (reduce (fn [{:keys [genome total-error] :as best} _]
                             (let [new-gn (vec (random-sample (rand) genome))
                                   new-indiv (assoc (individual-factory new-gn context) :genome new-gn)]
                               (if (<= (:total-error new-indiv) total-error)
                                 new-indiv
                                 best)))
                           individual
                           (range simplification-steps))]
    (log/info "POST-SIMPLIFICATION" simplified)
    simplified))
