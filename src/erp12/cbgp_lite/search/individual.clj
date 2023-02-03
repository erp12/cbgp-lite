(ns erp12.cbgp-lite.search.individual
  (:require [clj-fuzzy.levenshtein :as lev]
            [erp12.cbgp-lite.lang.ast :as a]
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

(defn invoke-func
  "Invokes the `func` on `args` and captures its stdout. Also captures exceptions."
  [func args]
  (try
    (with-out-and-stdout (apply func args))
    (catch Exception e
      {:output e :std-out nil})))

(defn errors-for-case
  "Compute errors on a single case given a program's output.

  Options:
      :case        - The case. Contains `output` and optionally `std-out`.
      :prog-output - A programs output containing `output` and optionally `std-out`.
      :penalty     - The penalty error for exceptions or nil output.
      :loss-fns    - A collection of loss functions for compute a single error."
  [{:keys [case prog-output penalty loss-fns]}]
  (let [{actual-output :output actual-stdout :std-out} prog-output
        {expected-output :output expected-stdout :std-out} case]
    (->> (conj
           ;; Compute loss values on returned value
           (mapv (fn [lf]
                   (if (or (nil? actual-output) (instance? Exception actual-output))
                     penalty
                     (lf actual-output expected-output)))
                 loss-fns)
           ;; Compute loss on printed output using string edit distance.
           (when (contains? case :std-out)
             (if (nil? actual-stdout)
               penalty
               (lev/distance actual-stdout expected-stdout))))
         (filter some?)
         vec)))

(defn evaluate-until-first-failure
  [{:keys [func cases loss-fns]}]
  (if (nil? func)
    {:cases-used 0}
    (loop [cases cases
           cases-used 0]
      (if (empty? cases)
        {:solution?  true
         :cases-used cases-used}
        (let [{:keys [inputs] :as case} (first cases)
              prog-output (invoke-func func inputs)
              errors (errors-for-case {:case        case
                                       :prog-output prog-output
                                       ;; Any positive number will short-circuit evaluation.
                                       :penalty     1
                                       :loss-fns    loss-fns})]
          (if (some pos? errors)
            (if-let [ex (when (instance? Exception (:output prog-output))
                          (:output prog-output))]
              {:cases-used (inc cases-used)
               :exception  ex
               :case       case}
              {:cases-used (inc cases-used)})
            (recur (rest cases)
                   (inc cases-used))))))))

(defn evaluate-full-behavior
  [{:keys [func cases loss-fns penalty]}]
  (if (nil? func)
    ;; If the compilation process did not produce any code
    ;; give penalty for all loss functions and std-out for each case.
    ;; May be too many errors (if no std-out) but they are all penalty, so that's okay.
    (let [errors (repeat (* (count cases) (inc (count loss-fns))) penalty)]
      {:func        nil
       :behavior    nil
       :errors      errors
       :total-error (reduce +' errors)
       :cases-used  0})
    (let [behavior (map #(invoke-func func (:inputs %)) cases)
          _ (log/debug "Behavior" behavior)
          ;; Compute the error on each case.
          errors (->> cases
                      (mapcat (fn [b case]
                                (errors-for-case {:case        case
                                                  :prog-output b
                                                  :loss-fns    loss-fns
                                                  :penalty     penalty}))
                              behavior)
                      ;; When there is no std-out error, filter out the nils.
                      (filter some?)
                      vec)
          _ (log/debug "Errors" errors)
          total-error (reduce +' errors)]
      {:behavior    behavior
       :errors      errors
       :total-error total-error
       :solution?   (zero? total-error)
       :cases-used  (count cases)
       :exception   (:output (first (filter #(instance? Exception (:output %)) behavior)))})))

(defn make-evaluator
  [{:keys [evaluate-fn cases arg-symbols] :as opts}]
  (fn [gn context]
    (log/debug "Evaluating genome" gn)
    (let [cases (or (:cases context) cases)
          _ (log/debug "Evaluating on" (count cases) "cases")
          ;; Get Push code from the genome.
          push (pl/plushy->push gn)
          _ (log/debug "Push" push)
          ;; Compile the Push into a Clojure form that accepts and returns the
          ;; correct types.
          ast (::c/ast (c/push->ast (assoc opts
                                      :push push
                                      :locals arg-symbols
                                      ;; @todo Experimental - record final stack AST sizes and types.
                                      ;; Disabled to reduce concurrent compilation coordination.
                                      :record-sketch? false)))
          _ (log/debug "AST" ast)
          form (when ast
                 (a/ast->form ast))
          _ (log/debug "Form" form)
          func (when form
                 (a/form->fn (vec arg-symbols) form))
          _ (log/debug "Function compiled" func)
          evaluation (try
                       (evaluate-fn (merge {:func func :cases cases} opts))
                       (catch Exception e
                         (throw (ex-info "Failed to evaluate Clojure form."
                                         {:code form}
                                         e))))]
      (merge {:push push
              :code form
              :func func}
             evaluation))))

(defn simplify
  [{:keys [individual simplification-steps evaluator context]}]
  (reduce (fn [{:keys [genome total-error] :as best} _]
            (let [new-gn (vec (random-sample (rand) genome))
                  new-indiv (assoc (evaluator new-gn context) :genome new-gn)]
              (if (<= (:total-error new-indiv) total-error)
                new-indiv
                best)))
          individual
          (range simplification-steps)))
