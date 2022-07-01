(ns erp12.cbgp-lite.benchmark.suite.psb
  (:require [clj-fuzzy.levenshtein :as lev]
            [clojure.string :as str]
            [clojure.string]
            [erp12.cbgp-lite.benchmark.utils :as bu]
            [psb2.core :as psb2]))

(defn problems
  [{:keys [penalty]}]
  {"checksum"
   {:input->type    {'input1 string?}
    :return-type    string?
    :other-types    [int? boolean? char?]
    :literals       ["Check sum is " \space 64]
    :lit-generators [(bu/int-generator 128)
                     #(rand-nth (concat [\newline \tab] (map char (range 32 127))))]
    :loss-fns       [lev/distance
                     #(if (not (empty? %1))
                        (Math/abs (- (int (last %2)) (int (last %1)))) ;distance from correct last character
                        penalty)]}

   "collatz-numbers"
   {:input->type    {'input1 int?}
    :return-type    int?
    :other-types    [int? float? boolean?]
    :literals       [0 1]
    :lit-generators [(bu/int-generator 100)]
    :loss-fns       [bu/absolute-distance]}

   "compare-string-lengths"
   {:input->type    {'input1 string?
                     'input2 string?
                     'input3 string?}
    :return-type    boolean?
    :other-types    [int?]
    :literals       []
    :lit-generators [bu/rand-bool]
    :loss-fns       [#(if (= %1 %2) 0 1)]}

   "count-odds"
   {:input->type    {'input1 [:vector int?]}
    :return-type    int?
    :other-types    [boolean?]
    :literals       [0 1 2]
    :lit-generators [(bu/int-generator 1000)]
    :loss-fns       [bu/absolute-distance]}

   "digits"
   {:input->type    {'input1 int?}
    :return-type    string?
    :other-types    [boolean? char?]
    :literals       [\newline]
    :lit-generators [(bu/int-generator 10)]
    :loss-fns       [lev/distance]}

   "double-letters"
   {:input->type    {'input1 string?}
    :return-type    string?
    :other-types    [int? boolean? char?]
    :literals       [\!]
    :lit-generators []
    :loss-fns       [lev/distance]}

   ;"even-squares"
   ;{:input->type    {'input1 int?}
   ; :return-type    string?
   ; :other-types    [int? boolean?]
   ; :literals       []
   ; :lit-generators []
   ; :loss-fns        [lev/distance
   ;                   #(+ (Math/abs (- ...))
   ;                       ..)
   ;                   #(let [])]}

   "for-loop-index"
   {:input->type    {'input1 int?
                     'input2 int?
                     'input3 int?}
    :return-type    string?
    :other-types    [int? boolean?]
    :literals       []
    :lit-generators []
    :loss-fns       [lev/distance]}

   "grade"
   {:input->type    {'input1 int?
                     'input2 int?
                     'input3 int?
                     'input4 int?
                     'input5 int?}
    :return-type    string?
    :other-types    [boolean?]
    :literals       ["Student has a"
                     " grade."
                     "A" "B" "C" "D" "F"]
    :lit-generators [#(rand-int 101)]
    :loss-fns       [lev/distance
                     ;; If correct format, distance from correct letter grade char.
                     (let [extract-letter #(second (re-find #"^Student has a (.) grade.$" %))]
                       #(let [actual-letter (extract-letter %1)
                              expected-letter (extract-letter %2)]
                          (if actual-letter
                            (Math/abs (- (int (first expected-letter))
                                         (int (first actual-letter))))
                            penalty)))]}

   "last-index-of-zero"
   {:input->type    {'input1 [:vector int?]}
    :return-type    int?
    :other-types    [boolean?]
    :literals       []
    :lit-generators [(bu/int-generator 50)]
    :loss-fns       [bu/absolute-distance]}

   "median"
   {:input->type    {'input1 int?
                     'input2 int?
                     'input3 int?}
    :return-type    int?
    :other-types    [boolean?]
    :literals       []
    :lit-generators [(bu/int-generator 100)]
    :loss-fns       [bu/absolute-distance]}

   "mirror-image"
   {:input->type    {'input1 [:vector int?]
                     'input2 [:vector int?]}
    :return-type    boolean?
    :other-types    [int?]
    :literals       []
    :lit-generators [bu/rand-bool]
    :loss-fns       [#(if (= %1 %2) 0 1)]}

   "negative-to-zero"
   {:input->type    {'input1 [:vector int?]}
    :return-type    [:vector int?]
    :other-types    [int? boolean?]
    :literals       []
    :lit-generators []
    :loss-fns       [lev/distance]}

   "number-io"
   {:input->type    {'input1 float?
                     'input2 int?}
    :return-type    string?
    :other-types    []
    :literals       []
    :lit-generators [(bu/int-generator 100)
                     #(- (rand 201.0) 100.0)]
    :loss-fns       [#(try
                        (bu/round 4 (Math/abs (- (Double/parseDouble %1) %2)))
                        (catch Exception e penalty))
                     #(lev/distance (take 10 %1)
                                    (take 10 (pr-str %2)))]}

   ; "pig-latin"

   "replace-space-with-newline"
   {:input->type    {'input1 string?}
    :return-type    int?
    ;; The `nil?` functions are side effects, like printing.
    :other-types    [int? boolean? char? nil?]
    :literals       [\space \newline]
    :lit-generators [#(rand-nth (concat [\newline \tab] (map char (range 32 127))))]
    :loss-fns       [bu/absolute-distance]
    ;; Config for how to unpack the cases from data files.
    :out-key        :output2
    :stdout-key     :output1}

   ; "scrabble-score"

   "small-or-large"
   {:input->type    {'input1 int?}
    :return-type    string?
    :other-types    [boolean?]
    :literals       ["small" "large"]
    :lit-generators [(bu/int-generator 10000)]
    :loss-fns       [lev/distance]}

   "smallest"
   {:input->type    {'input1 int?
                     'input2 int?
                     'input3 int?
                     'input4 int?}
    :return-type    int?
    :other-types    [boolean?]
    :literals       []
    :lit-generators [(bu/int-generator 100)]
    :loss-fns       [bu/absolute-distance]}

   ; "string-differences"

   "string-lengths-backwards"
   {:input->type    {'input1 [:vector string?]}
    :return-type    string?
    :other-types    [string? int? boolean? [:vector string?]]
    :literals       []
    :lit-generators [(bu/int-generator 100)]
    :loss-fns       [lev/distance]}

   ; "sum-of-squares"
   ; "super-anagrams"

   "syllables"
   {:input->type    {'input1 string?}
    :return-type    string?
    :other-types    [int? boolean? char?]
    :literals       ["The number of syllables is "
                     \a
                     \e
                     \i
                     \o
                     \u
                     \y
                     "aeiouy"]
    :lit-generators [;; Random visible character
                     #(rand-nth (concat [\newline \tab] (map char (range 32 127))))]
    :loss-fns       [lev/distance
                     (let [parse #(try (Integer/parseInt (last (str/split % #"\s+")))
                                       (catch Exception e nil))]
                       #(if-let [num (parse %1)]
                          (bu/absolute-distance num (parse %2))
                          penalty))]}

   "vector-average"
   {:input->type    {'input1 [:vector float?]}
    :return-type    float?
    :other-types    [int?]
    :literals       []
    :lit-generators []
    :loss-fns       [#(bu/round 4 (bu/absolute-distance %1 %2))]}

   "vectors-summed"
   {:input->type    {'input1 [:vector int?]
                     'input2 [:vector int?]}
    :return-type    [:vector int?]
    :other-types    [int?]
    :literals       []
    :lit-generators []
    :loss-fns       [#(reduce + (map bu/absolute-distance %1 %2))
                     #(* 1000 (bu/absolute-distance (count %1) (count %2)))]}

   ; "wallis-pi"
   ; "word-stats"
   ; "x-word-lines"

   })

(defn reshape-case
  [case {:keys [out-key stdout-key] :or {out-key :output1}}]
  (merge
    {:inputs (->> case
                  (filter (fn [[k _]] (str/starts-with? (name k) "input")))
                  (sort-by first)
                  (mapv second))
     :output (out-key case)}
    (when stdout-key
      {:std-out (stdout-key case)})))

(defn read-cases
  [{:keys [data-dir problem n-train n-test]}]
  (let [problem-info (get (problems {}) (name problem))
        reshape #(reshape-case % problem-info)
        {:keys [train test]} (psb2/fetch-examples (str data-dir) (str problem) n-train n-test)]
    {:train (map reshape train)
     :test  (map reshape test)}))
