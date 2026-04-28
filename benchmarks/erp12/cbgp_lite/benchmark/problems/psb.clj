(ns erp12.cbgp-lite.benchmark.problems.psb
  "Problem definitions wrapping the PSB2 program synthesis benchmark suite."
  (:require [clj-fuzzy.levenshtein :as lev]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [psb2.core :as psb2]
            [erp12.cbgp-lite.genome :as g]
            [erp12.cbgp-lite.individual :as i]
            [erp12.cbgp-lite.program.types :as t]
            [erp12.cbgp-lite.program.lib :as lib]
            [erp12.cbgp-lite.benchmark.utils :as u]))

(defn input-symbols
  "The default input symbols for PSB problems."
  [arity]
  (mapv #(symbol (str "input" (inc %))) (range arity)))

(defn reshape-case
  "Transforms a case from the `psb` library into a map for cbgp evaluation."
  [case {:keys [out-key stdout-key]
         :or   {out-key :output1}}]
  (merge {;; A vector of input values.
          :inputs (->> case
                       (filter (fn [[k _]] (str/starts-with? (name k) "input")))
                       (sort-by first)
                       (mapv second))
          ;; The output value. If multiple outputs, reshape into a tuple (vector).
          :output (if (sequential? out-key)
                    (vec (map #(get case %) out-key))
                    (out-key case))}
         ;; If one of the outputs should be taken from stdout, assign to to :std-out
         (when stdout-key
           {:std-out (stdout-key case)})))

(defn read-psb-cases
  "Reads training and test cases for the given problem."
  [{:keys [data-dir problem n-train n-test]
    :as   opts}]
  (let [reshape              #(reshape-case % opts)
        {:keys [train test]} (psb2/fetch-examples (str data-dir) (str problem) n-train n-test)]
    {:train (map reshape train)
     :test  (map reshape test)}))

(defn penalize-nil-loss
  [loss-fn penalty]
  (fn wrapped-loss [program-output correct-output]
    (if (u/has-nil? program-output)
      penalty
      (loss-fn program-output correct-output))))

(def DEFAULT-PENALTY 1e6)

(def problems
  {;; ;;;;;;;;;;;;;;;;
   ;; PSB1 Problems ;;
   ;; ;;;;;;;;;;;;;;;;

   "checksum"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit "Check sum is " t/STRING)
                     (g/->Lit \space t/CHAR)
                     (g/->Lit 64 t/INT)
                     (g/->LitGenerator (u/int-generator 128) t/INT)
                     (g/->LitGenerator u/rand-char t/CHAR)
                     (g/->Abs [t/CHAR] t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)
                     (penalize-nil-loss
                      #(if (seq %1)
                         (Math/abs (- (int (last %2)) (int (last %1))))
                         DEFAULT-PENALTY)
                      DEFAULT-PENALTY)]}

   "collatz-numbers"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/INT]
    :output-type    t/INT
    :type-ctors     #{t/INT t/FLOAT t/BOOL}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Lit 1 t/INT)
                     (g/->LitGenerator (u/int-generator 128) t/INT)
                     (g/->Abs [t/INT] t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]}

   "compare-string-lengths"
   {:input-symbols  (input-symbols 3)
    :input-types    [t/STRING t/STRING t/STRING]
    :output-type    t/BOOL
    :type-ctors     #{t/STRING t/BOOL t/INT}
    :extra-genes    [(g/->Lit true t/BOOL)
                     (g/->Lit false t/BOOL)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss #(if (= %1 %2) 0 1) DEFAULT-PENALTY)]}

   "count-odds"
   {:input-symbols  (input-symbols 1)
    :input-types    [(t/vec-type t/INT)]
    :output-type    t/INT
    :type-ctors     #{t/VECTOR t/INT t/BOOL}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Lit 1 t/INT)
                     (g/->Lit 2 t/INT)
                     (g/->LitGenerator (u/int-generator 1000) t/INT)
                     (g/->Abs [t/INT] t/BOOL)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]
    :solution       (list (g/var-gene {:sym 'input1})
                          (g/abs-gene {:param-types [t/INT]
                                       :ret-type    t/BOOL})
                          (g/lit-gene {:val 2 :typ t/INT})
                          (g/local-gene {:idx 0})
                          (g/var-gene {:sym `lib/int-mod})
                          (g/app-gene)
                          (g/lit-gene {:val 1 :typ t/INT})
                          (g/var-gene {:sym `=})
                          (g/app-gene)
                          :close
                          (g/var-gene {:sym `filterv})
                          (g/app-gene)
                          (g/var-gene {:sym `lib/count-vec})
                          (g/app-gene))}

   "digits"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/INT]
    :output-type    t/STRING
    :type-ctors     #{t/INT t/STRING t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit \newline t/CHAR)
                     (g/->LitGenerator (u/int-generator 10) t/INT)
                     (g/->Abs [t/CHAR] t/BOOL)
                     (g/->Abs [t/STRING] t/BOOL)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "double-letters"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR t/VECTOR}
    :extra-genes    [(g/->Lit \! t/CHAR)
                     (g/->Abs [t/CHAR] (t/vec-type t/CHAR))
                     (g/->Abs [t/STRING] (t/vec-type t/STRING))
                     (g/->Abs [t/CHAR] t/STRING)
                     (g/->Abs [t/STRING] t/STRING)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "even-squares"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/INT]
    :output-type    t/STRING
    :type-ctors     #{t/INT t/STRING t/BOOL}
    :extra-genes    [(g/->Abs [t/INT] t/BOOL)
                     (g/->Abs [t/INT] t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [;; Error 1: Levenshtein distance of printed strings
                     (penalize-nil-loss lev/distance DEFAULT-PENALTY)
                     ;; Error 2: Difference in number of lines with integer-parseable strings.
                     ;; Also, each line without an integer-parseable string contributes 1 error
                     (penalize-nil-loss
                      (fn [result correct-string]
                        (let [correct-integers (if (= correct-string "")
                                                 []
                                                 (map parse-long (str/split-lines correct-string)))
                              correct-number-lines (count correct-integers)
                              result-lines (if (= result "")
                                             []
                                             (str/split-lines result))
                              int-parse-strings (filter #(re-matches #"-?\d+" %) result-lines)
                              lines-with-integers (count int-parse-strings)
                              lines-without-integers (- (count result-lines) lines-with-integers)]
                          (+ (abs (- correct-number-lines lines-with-integers))
                             lines-without-integers)))
                      DEFAULT-PENALTY)
                     ;; Error 3: For each line in the result with a parseable integer, find the 
                     ;; integer error compared to correct integer. Sum these.
                     (penalize-nil-loss
                      (fn [result correct-string]
                        (let [correct-integers (if (= correct-string "")
                                                 []
                                                 (map parse-long (str/split-lines correct-string)))
                              result-lines (if (= result "")
                                             []
                                             (str/split-lines result))
                              int-parse-strings (filter #(re-matches #"-?\d+" %) result-lines)
                              pairs (map vector
                                         correct-integers
                                         (concat (map (fn [s]
                                                        (try (Integer/parseInt s)
                                                             (catch Exception _ :no-result)))
                                                       int-parse-strings)
                                                 (repeat :no-result)))]
                          (apply +' (map (fn [[cor res]]
                                           (if (not (number? res))
                                             ;; penalty for not enough lines with parseable integers
                                             100
                                             (abs (- cor res))))
                                         pairs))))
                      DEFAULT-PENALTY)]}

   "for-loop-index"
   {:input-symbols  (input-symbols 3)
    :input-types    [t/INT t/INT t/INT]
    :output-type    t/STRING
    :type-ctors     #{t/INT t/STRING t/BOOL}
    :extra-genes    [(g/->Lit "\n" t/STRING)
                     (g/->Lit \newline t/CHAR)
                     (g/->Abs [t/INT] t/BOOL)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "grade"
   {:input-symbols  (input-symbols 5)
    :input-types    [t/INT t/INT t/INT t/INT t/INT]
    :output-type    t/STRING
    :type-ctors     #{t/INT t/STRING t/BOOL}
    :extra-genes    [(g/->Lit "Student has a " t/STRING)
                     (g/->Lit " grade." t/STRING)
                     (g/->Lit "A" t/STRING)
                     (g/->Lit "B" t/STRING)
                     (g/->Lit "C" t/STRING)
                     (g/->Lit "D" t/STRING)
                     (g/->Lit "F" t/STRING)
                     (g/->LitGenerator #(rand-int 101) t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)
                     ;; If correct format, distance from correct letter grade char.
                     (penalize-nil-loss
                      (let [extract-letter #(second (re-find #"^Student has a (.) grade.$" %))]
                        (fn [actual expected]
                          (let [actual-letter   (extract-letter actual)
                                expected-letter (extract-letter expected)]
                            (if actual-letter
                              (Math/abs (- (int (first expected-letter))
                                           (int (first actual-letter))))
                              DEFAULT-PENALTY))))
                      DEFAULT-PENALTY)]}

   "last-index-of-zero"
   {:input-symbols  (input-symbols 1)
    :input-types    [(t/vec-type t/INT)]
    :output-type    t/INT
    :type-ctors     #{t/INT t/VECTOR t/BOOL}
    :extra-genes    [(g/->LitGenerator (u/int-generator 50) t/INT)
                     (g/->Abs [t/INT] t/BOOL)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]}

   "median"
   {:input-symbols  (input-symbols 3)
    :input-types    [t/INT t/INT t/INT]
    :output-type    t/INT
    :type-ctors     #{t/INT t/BOOL}
    :extra-genes    [(g/->LitGenerator (u/int-generator 100) t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]}

   "mirror-image"
   {:input-symbols  (input-symbols 2)
    :input-types    [(t/vec-type t/INT) (t/vec-type t/INT)]
    :output-type    t/BOOL
    :type-ctors     #{t/INT t/VECTOR t/BOOL}
    :extra-genes    [(g/->LitGenerator u/rand-bool t/BOOL)
                     (g/->Abs [t/INT] t/BOOL)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss #(if (= %1 %2) 0 1) DEFAULT-PENALTY)]
    :solution       (list (g/->Local 0)
                          (g/->Local 1)
                          (g/->Var `lib/vec-reverse)
                          (g/->App)
                          (g/->Var `=)
                          (g/->App))}

   "negative-to-zero"
   {:input-symbols  (input-symbols 1)
    :input-types    [(t/vec-type t/INT)]
    :output-type    (t/vec-type t/INT)
    :type-ctors     #{t/INT t/VECTOR t/BOOL}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Abs [t/INT] t/INT)
                     (g/->Abs [t/INT] t/BOOL)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "number-io"
   {:input-symbols  ['input1 'input2]
    :input-types    [t/FLOAT t/INT]
    :output-type    t/STRING
    :type-ctors     #{t/INT t/FLOAT t/STRING}
    :extra-genes    [(g/->LitGenerator (u/int-generator 100) t/INT)
                     (g/->LitGenerator #(- (rand 201.0) 100.0) t/FLOAT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss (fn number-io-loss1 [a e]
                                          (try
                                            (u/round 4 (Math/abs (- (Double/parseDouble a) e)))
                                            (catch Exception _ DEFAULT-PENALTY)))
                                        DEFAULT-PENALTY)
                     (penalize-nil-loss (fn number-io-loss2 [a e]
                                          (lev/distance (take 10 a)
                                                        (take 10 (pr-str e))))
                                        DEFAULT-PENALTY)]
    :solution       (list (g/->Var 'input1)
                          (g/->Var 'input2)
                          (g/->Var `float)
                          (g/->App)
                          (g/->Var `lib/float-add)
                          (g/->App)
                          (g/->Var `str)
                          (g/->App))}

   "pig-latin"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit \a t/CHAR)
                     (g/->Lit \e t/CHAR)
                     (g/->Lit \i t/CHAR)
                     (g/->Lit \o t/CHAR)
                     (g/->Lit \u t/CHAR)
                     (g/->Lit \space t/CHAR)
                     (g/->Lit "ay" t/STRING)
                     (g/->Lit "aeiou" t/STRING)
                     (g/->LitGenerator u/rand-char t/CHAR)
                     (g/->LitGenerator (u/string-generator 21) t/STRING)
                     (g/->Abs [t/STRING] t/STRING)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "replace-space-with-newline"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/INT
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR t/NIL}
    :extra-genes    [(g/->Lit \space t/CHAR)
                     (g/->Lit \newline t/CHAR)
                     (g/->LitGenerator u/rand-char t/CHAR)
                     (g/->Abs [t/CHAR] t/CHAR)
                     (g/->Abs [t/STRING] t/STRING)
                     (g/->Abs [t/CHAR] t/INT)
                     (g/->Abs [t/STRING] t/INT)]

    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]
    ;; Config for how to unpack the cases from data files.
    :out-key        :output2
    :stdout-key     :output1
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY}

   "scrabble-score"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/INT
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR t/VECTOR}
    :extra-genes    [(g/->Lit (let [scores {\a 1, \b 3, \c 3, \d 2, \e 1, \f 4, \g 2, \h 4, \i 1,
                                            \j 8, \k 5, \l 1, \m 3, \n 1, \o 1, \p 3, \q 10, \r 1,
                                            \s 1, \t 1, \u 1, \v 4, \w 4, \x 8, \y 4, \z 10}]
                               (vec (for [c (map char (range 0 127))]
                                      (get scores (first (str/lower-case c)) 0))))
                              (t/vec-type t/INT))
                     (g/->Abs [t/STRING] t/INT)
                     (g/->Abs [t/CHAR] t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]}

   "small-or-large"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/INT]
    :output-type    t/STRING
    :type-ctors     #{t/INT t/STRING t/BOOL}
    :extra-genes    [(g/->Lit "small" t/STRING)
                     (g/->Lit "large" t/STRING)
                     (g/->LitGenerator (u/int-generator 10000) t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "smallest"
   {:input-symbols  (input-symbols 4)
    :input-types    [t/INT t/INT t/INT t/INT]
    :output-type    t/INT
    :type-ctors     #{t/INT t/BOOL}
    :extra-genes    [(g/->LitGenerator (u/int-generator 100) t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]
    :solution       (list (g/local-gene {:idx 0})
                          (g/local-gene {:idx 1})
                          (g/var-gene {:sym `lib/min'})
                          (g/app-gene)
                          (g/local-gene {:idx 2})
                          (g/var-gene {:sym `lib/min'})
                          (g/app-gene)
                          (g/local-gene {:idx 3})
                          (g/var-gene {:sym `lib/min'})
                          (g/app-gene))}

   "string-differences"
   {:input-symbols  (input-symbols 2)
    :input-types    [t/STRING t/STRING]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit \space t/CHAR)
                     (g/->Lit \newline t/CHAR)
                     (g/->LitGenerator (u/int-generator 10) t/INT)
                     (g/->Abs [t/STRING t/STRING] t/BOOL)
                     (g/->Abs [t/CHAR t/CHAR] t/BOOL)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)
                     (penalize-nil-loss
                      (fn [result correct-output]
                        (abs (- (count (re-seq #"(?m)^\d+ \S \S$" correct-output))
                                (count (re-seq #"(?m)^\d+ \S \S$" result)))))
                      DEFAULT-PENALTY)]}

   "string-lengths-backwards"
   {:input-symbols  (input-symbols 1)
    :input-types    [(t/vec-type t/STRING)]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/INT t/VECTOR t/BOOL}
    :extra-genes    [(g/->LitGenerator (u/int-generator 100) t/INT)
                     (g/->Abs [t/STRING] t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "sum-of-squares"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/INT]
    :output-type    t/INT
    :type-ctors     #{t/INT t/BOOL}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Lit 1 t/INT)
                     (g/->LitGenerator (u/int-generator 100) t/INT)
                     (g/->Abs [t/INT] t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]}

   "super-anagrams"
   {:input-symbols  (input-symbols 2)
    :input-types    [t/STRING t/STRING]
    :output-type    t/BOOL
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->LitGenerator u/rand-bool t/BOOL)
                     (g/->LitGenerator (u/int-generator 1000) t/INT)
                     (g/->LitGenerator u/rand-char t/CHAR)
                     (g/->Abs [t/STRING] t/INT)
                     (g/->Abs [t/CHAR] t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss #(if (= %1 %2) 0 1) DEFAULT-PENALTY)]}

   "syllables"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit "The number of syllables is " t/STRING)
                     (g/->Lit \a t/CHAR)
                     (g/->Lit \e t/CHAR)
                     (g/->Lit \i t/CHAR)
                     (g/->Lit \o t/CHAR)
                     (g/->Lit \u t/CHAR)
                     (g/->Lit \y t/CHAR)
                     (g/->Lit "aeiouy" t/STRING)
                     (g/->LitGenerator u/rand-char t/CHAR)
                     (g/->Abs [t/CHAR] t/INT)
                     (g/->Abs [t/CHAR] t/BOOL)
                     (g/->Abs [t/STRING] t/INT)
                     (g/->Abs [t/STRING] t/BOOL)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)
                     (penalize-nil-loss
                      (let [parse #(try (Integer/parseInt (last (str/split % #"\s+")))
                                        (catch Exception _ nil))]
                        (fn [actual expected]
                          (if-let [num (parse actual)]
                            (u/absolute-distance num (parse expected))
                            DEFAULT-PENALTY)))
                      DEFAULT-PENALTY)]}

   "vector-average"
   {:input-symbols  (input-symbols 1)
    :input-types    [(t/vec-type t/FLOAT)]
    :output-type    t/FLOAT
    :type-ctors     #{t/FLOAT t/INT t/VECTOR}
    :extra-genes    [(g/->Abs [t/FLOAT t/FLOAT] t/FLOAT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss #(u/round 4 (u/absolute-distance %1 %2)) DEFAULT-PENALTY)]}

   "vectors-summed"
   {:input-symbols  ['input1 'input2]
    :input-types    [(t/vec-type t/INT) (t/vec-type t/INT)]
    :output-type    (t/vec-type t/INT)
    :type-ctors     [t/INT t/VECTOR]
    :extra-genes    [(g/->Lit [] (t/vec-type t/INT))
                     (g/->LitGenerator (u/int-generator 1000) t/INT)
                     (g/->Abs [t/INT t/INT] t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss (fn loss1 [y-hat y]
                                          (reduce +' (map #(or (u/absolute-distance %1 %2) DEFAULT-PENALTY)
                                                          y-hat y)))
                                        DEFAULT-PENALTY)
                     (penalize-nil-loss (fn loss2 [y-hat y]
                                          (*' 1000 (u/absolute-distance (count y-hat) (count y))))
                                        DEFAULT-PENALTY)]
    :solution       (list (g/var-gene {:sym 'input1})
                          (g/var-gene {:sym 'input2})
                          (g/var-gene {:sym `lib/int-add})
                          (g/var-gene {:sym `lib/mapv2})
                          (g/app-gene))}

   ; "wallis-pi"
   ; "word-stats"

   "x-word-lines"
   {:input-symbols  ['input1 'input2]
    :input-types    [t/STRING t/INT]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit \newline t/CHAR)
                     (g/->Lit \space t/CHAR)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)
                     (penalize-nil-loss
                      (fn [result correct-output]
                        (abs (- (count (filter #(= % \newline) correct-output))
                                (count (filter #(= % \newline) result)))))
                      DEFAULT-PENALTY)
                     (penalize-nil-loss
                      (fn [result correct-output]
                        (let [result-lines  (str/split-lines result)
                              words-per-line (if (empty? result-lines)
                                               0
                                               (count (str/split (first result-lines) #"\s+")))]
                          (+ (apply + (map #(abs (- words-per-line
                                                    (count (str/split (str/trim %) #"\s+"))))
                                           (butlast result-lines)))
                             (let [last-correct (let [l (last (str/split-lines correct-output))]
                                                  (if l l ""))
                                   last-result  (let [l (last result-lines)]
                                                  (if l l ""))]
                               (abs (- (count (str/split (str/trim last-correct) #"\s+"))
                                       (count (str/split (str/trim last-result) #"\s+"))))))))
                      DEFAULT-PENALTY)]}

   ;; ;;;;;;;;;;;;;;;;
   ;; PSB2 Problems ;;
   ;; ;;;;;;;;;;;;;;;;

   "basement"
   {:input-symbols  (input-symbols 1)
    :input-types    [(t/vec-type t/INT)]
    :output-type    t/INT
    :type-ctors     #{t/INT t/VECTOR t/BOOL}
    :extra-genes    [(g/->Lit [] (t/vec-type t/INT))
                     (g/->Lit -1 t/INT)
                     (g/->Lit 0 t/INT)
                     (g/->Lit 1 t/INT)
                     (g/->LitGenerator (u/int-generator 1000) t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]}

   "bouncing-balls"
   {:input-symbols  (input-symbols 3)
    :input-types    [t/FLOAT t/FLOAT t/INT]
    :output-type    t/FLOAT
    :type-ctors     #{t/FLOAT t/INT t/BOOL}
    :extra-genes    [(g/->Lit 0.0 t/FLOAT)
                     (g/->Lit 1.0 t/FLOAT)
                     (g/->Lit 2.0 t/FLOAT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss #(u/round 3 (u/absolute-distance %1 %2)) DEFAULT-PENALTY)]}

   "bowling"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/INT
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit \- t/CHAR)
                     (g/->Lit \X t/CHAR)
                     (g/->Lit \/ t/CHAR)
                     (g/->Lit \1 t/CHAR)
                     (g/->Lit \2 t/CHAR)
                     (g/->Lit \3 t/CHAR)
                     (g/->Lit \4 t/CHAR)
                     (g/->Lit \5 t/CHAR)
                     (g/->Lit \6 t/CHAR)
                     (g/->Lit \7 t/CHAR)
                     (g/->Lit \8 t/CHAR)
                     (g/->Lit \9 t/CHAR)
                     (g/->Lit 10 t/INT)
                     (g/->LitGenerator (u/int-generator 10) t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]}

   "camel-case"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit \- t/CHAR)
                     (g/->Lit \space t/CHAR)
                     (g/->LitGenerator u/rand-char t/CHAR)
                     (g/->LitGenerator (u/string-generator 21) t/STRING)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   ;; "coin-sums" ;; NEEDS MULTIPLE OUTPUTS

   "cut-vector"
   {:input-symbols  (input-symbols 1)
    :input-types    [(t/vec-type t/INT)]
    :output-type    (t/tuple-type [(t/vec-type t/INT) (t/vec-type t/INT)])
    :out-key        [:output1 :output2]
    :type-ctors     #{t/INT t/VECTOR t/BOOL (t/tuple-ctor 2)}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Lit [] (t/vec-type t/INT))
                     (g/->LitGenerator (fn [] (vec (repeatedly (rand-int 21) #(inc (rand-int 10000)))))
                                       (t/vec-type t/INT))]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss #(u/vector-of-numbers-loss (first %1) (first %2)) DEFAULT-PENALTY)
                     (penalize-nil-loss #(u/vector-of-numbers-loss (second %1) (second %2)) DEFAULT-PENALTY)]}

   "dice-game"
   {:input-symbols  (input-symbols 2)
    :input-types    [t/INT t/INT]
    :output-type    t/FLOAT
    :type-ctors     #{t/INT t/FLOAT t/BOOL}
    :extra-genes    [(g/->Lit 0.0 t/FLOAT)
                     (g/->Lit 1.0 t/FLOAT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss #(u/round 3 (u/absolute-distance %1 %2)) DEFAULT-PENALTY)]}

   "find-pair"
   {:input-symbols  (input-symbols 2)
    :input-types    [(t/vec-type t/INT) t/INT]
    :output-type    (t/tuple-type [t/INT t/INT])
    :out-key        [:output1 :output2]
    :type-ctors     #{t/INT t/VECTOR t/BOOL (t/tuple-ctor 2)}
    :extra-genes    [(g/->Lit -1 t/INT)
                     (g/->Lit 0 t/INT)
                     (g/->Lit 1 t/INT)
                     (g/->Lit 2 t/INT)
                     (g/->LitGenerator (u/int-generator 1000) t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss #(u/absolute-distance (first %1) (first %2)) DEFAULT-PENALTY)
                     (penalize-nil-loss #(u/absolute-distance (second %1) (second %2)) DEFAULT-PENALTY)]}

   "fizz-buzz"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/INT]
    :output-type    t/STRING
    :type-ctors     #{t/INT t/STRING t/BOOL}
    :extra-genes    [(g/->Lit "Fizz" t/STRING)
                     (g/->Lit "Buzz" t/STRING)
                     (g/->Lit "FizzBuzz" t/STRING)
                     (g/->Lit 0 t/INT)
                     (g/->Lit 3 t/INT)
                     (g/->Lit 5 t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "fuel-cost"
   {:input-symbols  ['input1]
    :input-types    [(t/vec-type t/INT)]
    :output-type    t/INT
    :type-ctors     [t/VECTOR t/INT t/BOOL]
    :extra-genes    [;; (g/->LitGenerator (u/int-generator 1000) t/INT)
                     (g/->Lit 0 t/INT)
                     (g/->Lit 1 t/INT)
                     (g/->Lit 2 t/INT)
                     (g/->Lit 3 t/INT)
                     (g/->Abs [t/INT] t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]
    :solution       (list (g/var-gene {:sym 'input1})
                          (g/abs-gene {:param-types [t/INT] :ret-type t/INT})
                          (g/lit-gene {:val 2 :typ t/INT})
                          (g/lit-gene {:val 3 :typ t/INT})
                          (g/local-gene {:idx 0})
                          (g/var-gene {:sym `lib/int-quot})
                          (g/app-gene)
                          (g/var-gene {:sym `lib/int-sub})
                          (g/app-gene)
                          :close
                          (g/var-gene {:sym `mapv})
                          (g/app-gene)
                          (g/var-gene {:sym `lib/int-add})
                          (g/var-gene {:sym `lib/reduce-vec})
                          (g/app-gene))}

   "gcd"
   {:input-symbols  (input-symbols 2)
    :input-types    [t/INT t/INT]
    :output-type    t/INT
    :type-ctors     #{t/INT t/BOOL}
    :extra-genes    [(g/->LitGenerator (u/int-generator 10) t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]}

   "indices-of-substring"
   {:input-symbols  (input-symbols 2)
    :input-types    [t/STRING t/STRING]
    :output-type    (t/vec-type t/INT)
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR t/VECTOR}
    :extra-genes    [(g/->Lit [] (t/vec-type t/INT))
                     (g/->Lit "" t/STRING)
                     (g/->Lit 0 t/INT)
                     (g/->Lit 1 t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "leaders"
   {:input-symbols  (input-symbols 1)
    :input-types    [(t/vec-type t/INT)]
    :output-type    (t/vec-type t/INT)
    :type-ctors     #{t/INT t/VECTOR t/BOOL}
    :extra-genes    [(g/->Lit [] (t/vec-type t/INT))
                     (g/->LitGenerator (fn [] (vec (repeatedly (rand-int 21) #(rand-int 1001))))
                                       (t/vec-type t/INT))]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/vector-of-numbers-loss DEFAULT-PENALTY)]}

   "luhn"
   {:input-symbols  (input-symbols 1)
    :input-types    [(t/vec-type t/INT)]
    :output-type    t/INT
    :type-ctors     #{t/INT t/VECTOR t/BOOL}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Lit 2 t/INT)
                     (g/->Lit 9 t/INT)
                     (g/->Lit 10 t/INT)
                     (g/->LitGenerator (u/int-generator 10) t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]}

   ;; "mastermind" ;; NEEDS MULTIPLE OUTPUTS

   "middle-character"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit "" t/STRING)
                     (g/->Lit 0 t/INT)
                     (g/->Lit 1 t/INT)
                     (g/->Lit 2 t/INT)
                     (g/->LitGenerator (u/int-generator 100) t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "paired-digits"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/INT
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->LitGenerator (fn [] (rand-nth "0123456789")) t/CHAR)
                     (g/->LitGenerator (fn [] (rand-int 10)) t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss u/absolute-distance DEFAULT-PENALTY)]}

   "shopping-list"
   {:input-symbols  (input-symbols 2)
    :input-types    [(t/vec-type t/FLOAT) (t/vec-type t/FLOAT)]
    :output-type    t/FLOAT
    :type-ctors     #{t/FLOAT t/INT t/VECTOR t/BOOL}
    :extra-genes    [(g/->Lit 0.0 t/FLOAT)
                     (g/->Lit 100.0 t/FLOAT)
                     (g/->LitGenerator (fn [] (* (rand) 100)) t/FLOAT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss #(u/round 2 (u/absolute-distance %1 %2)) DEFAULT-PENALTY)]}

   "snow-day"
   {:input-symbols  (input-symbols 4)
    :input-types    [t/INT t/FLOAT t/FLOAT t/FLOAT]
    :output-type    t/FLOAT
    :type-ctors     #{t/INT t/FLOAT t/BOOL}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Lit 1 t/INT)
                     (g/->Lit -1 t/INT)
                     (g/->Lit 0.0 t/FLOAT)
                     (g/->Lit 1.0 t/FLOAT)
                     (g/->Lit -1.0 t/FLOAT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss #(u/round 3 (u/absolute-distance %1 %2)) DEFAULT-PENALTY)]}

   "solve-boolean"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/BOOL
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit true t/BOOL)
                     (g/->Lit false t/BOOL)
                     (g/->Lit \t t/CHAR)
                     (g/->Lit \f t/CHAR)
                     (g/->Lit \| t/CHAR)
                     (g/->Lit \& t/CHAR)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss #(if (= %1 %2) 0 1) DEFAULT-PENALTY)]}

   "spin-words"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit 4 t/INT)
                     (g/->Lit 5 t/INT)
                     (g/->Lit \space t/CHAR)
                     (g/->LitGenerator u/rand-char t/CHAR)
                     (g/->LitGenerator (u/string-generator 21) t/STRING)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "square-digits"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/INT]
    :output-type    t/STRING
    :type-ctors     #{t/INT t/STRING t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit "" t/STRING)
                     (g/->Lit 0 t/INT)
                     (g/->Lit 1 t/INT)
                     (g/->Lit 2 t/INT)
                     (g/->LitGenerator (u/int-generator 100) t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "substitution-cipher"
   {:input-symbols  (input-symbols 3)
    :input-types    [t/STRING t/STRING t/STRING]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit "" t/STRING)
                     (g/->Lit 0 t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "twitter"
   {:input-symbols  (input-symbols 1)
    :input-types    [t/STRING]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/INT t/BOOL t/CHAR}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Lit 140 t/INT)
                     (g/->Lit "Too many characters" t/STRING)
                     (g/->Lit "You didn't type anything" t/STRING)
                     (g/->Lit "Your tweet has " t/STRING)
                     (g/->Lit " characters" t/STRING)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss lev/distance DEFAULT-PENALTY)]}

   "vector-distance"
   {:input-symbols  (input-symbols 2)
    :input-types    [(t/vec-type t/FLOAT) (t/vec-type t/FLOAT)]
    :output-type    t/FLOAT
    :type-ctors     #{t/FLOAT t/INT t/VECTOR t/BOOL}
    :extra-genes    [(g/->Lit [] (t/vec-type t/FLOAT))
                     (g/->Lit 0 t/INT)]
    :dataset-reader read-psb-cases
    :penalty        DEFAULT-PENALTY
    :loss-fns       [(penalize-nil-loss #(u/round 3 (u/absolute-distance %1 %2)) DEFAULT-PENALTY)]}})

(defn validate-solutions
  [{:keys [data-dir num-cases penalty hooks]
    :or   {penalty 1000
           hooks   {}}}]
  (doseq [[problem-name problem-metadata] (filter (fn [[_ md]] (contains? md :solution)) problems)]
    (log/info "Starting" problem-name)
    (let [evaluator  (i/make-genome-evaluator (assoc problem-metadata
                                                     :cases (:test (read-psb-cases {:data-dir data-dir
                                                                                    :problem  problem-name
                                                                                    :n-train  0
                                                                                    :n-test   num-cases}))
                                                     :penalty penalty
                                                     :hooks hooks))
          start-time (System/currentTimeMillis)
          evaluation (evaluator (:solution problem-metadata) nil)
          duration   (/ (- (System/currentTimeMillis) start-time) 1000.0)]
      (cond
        (> (:total-error evaluation) 0)
        (throw (ex-info (str problem-name " solution has non-zero error.") {:eval evaluation}))

        (some? (:exception evaluation))
        (throw (ex-info (str problem-name " solution threw an error.") {:eval evaluation} (:exception evaluation)))

        :else
        (log/info problem-name "passed in" duration "seconds.")))))

(comment

  (validate-solutions {:data-dir  "data/psb/"
                       :num-cases 200})

  *e

  (comment))
