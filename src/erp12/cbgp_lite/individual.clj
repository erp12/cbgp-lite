(ns erp12.cbgp-lite.individual
  "Compiles genomes into executable functions and evaluates them against training cases to produce individuals."
  (:require [clj-fuzzy.levenshtein :as lev]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [erp12.cbgp-lite.genome :refer [plushy->push]]
            [erp12.cbgp-lite.program.compile :refer [push->ast]]
            [erp12.cbgp-lite.program.expr :as e]
            [erp12.cbgp-lite.program.lib :as lib]
            [erp12.cbgp-lite.program.types :as t])
  (:import (java.io StringWriter)))

(defmacro with-out-and-stdout
  "Evaluates the form and returns a map containing
     :output - The value returned by the form
     :std-out - A string of any text printed to stdout when evaluating the form"
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
      ;; Only keep a summary of the error because the stack traces are large and cause OOM.
      {:exception (str (.getSimpleName (class e)) ": " (.getMessage e))
       :std-out   nil})))

(defn errors-for-case
  "Compute errors on a single case given a program's output.

  Options:
      :actual     - A programs output containing `:output` or `:exception` and optionally `:std-out`.
      :expected   - The ground truth case. Contains `:output` and optionally `:std-out`.
      :penalty    - The penalty error for exceptions or nil output.
      :loss-fns   - A collection of loss functions."
  [{:keys [actual expected penalty loss-fns]}]
  (->> (conj
        ;; Compute loss values on returned value
        (mapv (fn [lf]
                ;; Penalty if the program exited with an exception or returned nil.
                (if (or (contains? actual :exception) (nil? (:output actual)))
                  penalty
                  (lf (:output actual) (:output expected))))
              loss-fns)
        ;; Compute loss on printed output using string edit distance.
        (when (contains? expected :std-out)
          (if (nil? (:std-out actual))
            penalty
            (lev/distance (:std-out actual) (:std-out expected)))))
       (filter some?)
       (vec)))

(defn evaluate-func
  "Evaluates the given function by computing an error for every loss function on every case."
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
    (let [behavior    (mapv #(invoke-func func (:inputs %)) cases)
          ;; Compute the error on each case.
          errors      (->> cases
                           (mapcat (fn [b case]
                                     (errors-for-case {:actual   b
                                                       :expected case
                                                       :loss-fns loss-fns
                                                       :penalty  penalty}))
                                   behavior)
                           ;; When there is no std-out error, filter out the nils.
                           (filter some?)
                           (vec))
          total-error (reduce +' errors)]
      {:behavior     behavior
       :errors       errors
       :total-error  total-error
       :solution?    (zero? total-error)
       :cases-used   (count cases)
       ;; Save the message of the first exception thrown for debugging.
       :exception    (some :exception behavior)
       ;; Record true if any of the exceptions include the error when memory limit is exceeded.
       :exceeded-mem (boolean (seq (filter #(str/starts-with? (or (:exception %) "")
                                                              "ExceptionInfo: Value too large")
                                           behavior)))})))

(defn make-genome-evaluator
  "Create an evaluation function that takes a genome as input and returns a fully evaluated individual map."
  [{:keys [input-symbols input-types output-type hooks]
    :as   opts}]
  (assert (= (count input-symbols)
             (count input-types))
          "Number of input symbols must match number of input types.")
  (let [hook (:eval hooks)
        input-schemes (mapv #(t/generalize % lib/type-env) input-types)
        type-env      (into lib/type-env (zipmap input-symbols input-schemes))]
    (fn genome-evaluator [genome context]
      (log/debug "Evaluating genome"  genome)
      (let [;; Get the cases from the context, or fallback on the options.
            ;; Context typically contains the cases when downsampling a batch each generation.
            cases      (or (:cases context) (:cases opts))
            _          (log/debug "Evaluating on" (count cases) "cases")
            ;; Translate the plushy genome into Push code.
            push       (plushy->push genome)
            _          (log/debug "Push" push)
            ;; Compile the Push into a Clojure form that accepts and returns the
            ;; correct types.
            expr       (:expr (push->ast {:push push
                                          :output-type output-type
                                          :type-env type-env
                                          :hooks hooks}))
            _          (log/debug "AST expr" expr)
            form       (when expr
                         (e/to-form expr))
            _          (log/debug "Form" form)
            ;; When debugging evaluation, disable concurrency and uncomment this line to examine problematic functions.
            ;; _          (spit "form.clj" form)
            func       (when form
                         (e/form->fn input-symbols form))
            _          (log/debug "Function" func)
            ;; Evaluates the function, 
            evaluation (try
                         (evaluate-func (merge {:func  func
                                                :cases cases}
                                               opts))
                         (catch Exception e
                           (throw (ex-info "Failed to evaluate Clojure form."
                                           {:code form}
                                           e))))
            individual (merge {:push push
                               ;; :expr expr
                               :code form
                               :func func}
                              evaluation)]
        (when hook
          (hook individual))
        individual))))

(defn simplify
  "Simplifies the individual by shrinking the genome without allowing the total error to get worse."
  [{:keys [individual simplification-steps evaluator]}]
  (reduce (fn [{:keys [genome total-error]
                :as   best} _]
            (let [new-genome     (vec (random-sample (+ 0.5 (rand 0.5)) genome))
                  new-individual (assoc (evaluator new-genome {}) :genome new-genome)]
              (if (<= (:total-error new-individual) total-error)
                new-individual
                best)))
          individual
          (range simplification-steps)))