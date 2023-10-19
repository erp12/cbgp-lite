(ns erp12.cbgp-lite.benchmark.suite.psb
  (:require [clj-fuzzy.levenshtein :as lev]
            [clojure.string :as str]
            [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.search.individual :as i]
            [erp12.cbgp-lite.task :as task]
            [psb2.core :as psb2]))

(defn problems
  [{:keys [penalty]}]
  (let [penalize-nil (fn [loss-fn]
                       (fn wrapped-loss [program-output correct-output]
                         (if (bu/has-nil? program-output)
                           penalty
                           (loss-fn program-output correct-output))))]
    (update-vals
     {"checksum" {:input->type {'input1 {:type 'string?}}
                  :ret-type {:type 'string?}
                  :other-type-ctors #{'int? 'boolean? 'char?}
                  :extra-genes [{:gene :lit
                                 :val "Check sum is "
                                 :type {:type 'string?}}
                                {:gene :lit
                                 :val \space
                                 :type {:type 'char?}}
                                {:gene :lit
                                 :val 64
                                 :type {:type 'int?}}
                                {:gene :lit-generator
                                 :fn (bu/int-generator 128)
                                 :type {:type 'int?}}
                                {:gene :lit-generator
                                 :fn bu/rand-char
                                 :type {:type 'char?}}
                                {:gene :fn
                                 :arg-types [{:type 'char?}]
                                 :ret-type {:type 'int?}}]
                  :loss-fns [lev/distance
                             #(if (seq %1)
                                ;; distance from correct last character
                                (Math/abs (- (int (last %2)) (int (last %1))))
                                penalty)]}

      "collatz-numbers" {:input->type {'input1 {:type 'int?}}
                         :ret-type {:type 'int?}
                         :other-type-ctors #{'double? 'boolean?}
                         :extra-genes [{:gene :lit
                                        :val 0
                                        :type {:type 'int?}}
                                       {:gene :lit
                                        :val 1
                                        :type {:type 'int?}}
                                       {:gene :lit-generator
                                        :fn (bu/int-generator 128)
                                        :type {:type 'int?}}
                                       {:gene :fn
                                        :arg-types [{:type 'int?}]
                                        :ret-type {:type 'int?}}]
                         :loss-fns [bu/absolute-distance]}

      "compare-string-lengths" {:input->type {'input1 {:type 'string?}
                                              'input2 {:type 'string?}
                                              'input3 {:type 'string?}}
                                :ret-type {:type 'boolean?}
                                :other-type-ctors #{'int?}
                                :extra-genes [{:gene :lit-generator
                                               :fn bu/rand-bool
                                               :type {:type 'boolean?}}]
                                :loss-fns [#(if (= %1 %2) 0 1)]}

      "count-odds" {:input->type {'input1 {:type :vector
                                           :child {:type 'int?}}}
                    :ret-type {:type 'int?}
                    :other-type-ctors #{'boolean?}
                    :extra-genes [{:gene :lit
                                   :val 0
                                   :type {:type 'int?}}
                                  {:gene :lit
                                   :val 1
                                   :type {:type 'int?}}
                                  {:gene :lit
                                   :val 2
                                   :type {:type 'int?}}
                                  {:gene :lit-generator
                                   :fn (bu/int-generator 1000)
                                   :type {:type 'int?}}
                                  {:gene :fn
                                   :arg-types [lib/INT]
                                   :ret-type lib/BOOLEAN}]
                    :loss-fns [bu/absolute-distance]
                    :solution (list {:gene :local
                                     :idx 0}
                                    {:gene :fn
                                     :arg-types [lib/INT]
                                     :ret-type lib/BOOLEAN}
                                    {:gene :lit
                                     :val 2
                                     :type {:type 'int?}}
                                    {:gene :local
                                     :idx 1}
                                    {:gene :var
                                     :name 'int-mod}
                                    {:gene :apply}
                                    {:gene :lit
                                     :val 1
                                     :type {:type 'int?}}
                                    {:gene :var
                                     :name '=}
                                    {:gene :apply}
                                    {:gene :close}
                                    {:gene :var
                                     :name 'filterv}
                                    {:gene :apply}
                                    {:gene :var
                                     :name 'count-vec}
                                    {:gene :apply})}

      "digits" {:input->type {'input1 {:type 'int?}}
                :ret-type {:type 'string?}
                :other-type-ctors #{'boolean? 'char?}
                :extra-genes [{:gene :lit
                               :val \newline
                               :type {:type 'char?}}
                              {:gene :lit
                               :fn (bu/int-generator 10)
                               :type {:type 'int?}}
                              {:gene :fn
                               :arg-types [lib/CHAR]
                               :ret-type lib/BOOLEAN}
                              {:gene :fn
                               :arg-types [lib/STRING]
                               :ret-type lib/BOOLEAN}]
                :loss-fns [lev/distance]}

      "double-letters" {:input->type {'input1 {:type 'string?}}
                        :ret-type {:type 'string?}
                        :other-type-ctors #{'int? 'boolean? 'char?}
                        :extra-genes [{:gene :lit
                                       :val \!
                                       :type {:type 'char?}}
                                      {:gene :fn
                                       :arg-types [lib/CHAR]
                                       :ret-type (lib/vector-of lib/CHAR)}
                                      {:gene :fn
                                       :arg-types [lib/STRING]
                                       :ret-type (lib/vector-of lib/STRING)}
                                      {:gene :fn
                                       :arg-types [lib/CHAR]
                                       :ret-type lib/STRING}
                                      {:gene :fn
                                       :arg-types [lib/STRING]
                                       :ret-type lib/STRING}]
                        :loss-fns [lev/distance]}

      "even-squares" {:input->type {'input1 {:type 'int?}}
                      :ret-type {:type 'string?}
                      :other-type-ctors #{'boolean?}
                      :extra-genes [{:gene :fn
                                     :arg-types [lib/INT]
                                     :ret-type lib/BOOLEAN}
                                    {:gene :fn
                                     :arg-types [lib/INT]
                                     :ret-type lib/INT}]
                      :loss-fns [;; Error 1: Levenshtein distance of printed strings
                                 lev/distance
                                 ;; Error 2: Difference in number of lines with integer-parseable strings.
                                 ;; Also, each line without an integer-parseable string contributes 1 error
                                 (fn [result correct-string]
                                   (let [correct-integers (if (= correct-string "")
                                                            []
                                                            (map parse-long (str/split-lines correct-string)))
                                         correct-number-lines (count correct-integers)
                                         result-lines (if (= result "")
                                                        []
                                                        (str/split-lines result))
                                         int-parse-strings (filter #(re-matches #"-?\d+" %) result-lines)
                                         lines-with-integer-parseable-strings (count int-parse-strings)
                                         lines-without-integer-parseable-strings (- (count result-lines) lines-with-integer-parseable-strings)]
                                     (+ (abs (- correct-number-lines lines-with-integer-parseable-strings))
                                        lines-without-integer-parseable-strings)))
                                 ;; Error 3: For each line in the result with a parseable integer, find the 
                                 ;; integer error compared to correct integer. Sum these.
                                 (fn [result correct-string]
                                   (let [correct-integers (if (= correct-string "")
                                                            []
                                                            (map parse-long (str/split-lines correct-string)))
                                         result-lines (if (= result "")
                                                        []
                                                        (str/split-lines result))
                                         int-parse-strings (filter #(re-matches #"-?\d+" %) result-lines)
                                         correct-result-int-pairs (map vector
                                                                       correct-integers
                                                                       (concat (map (fn [int-str]
                                                                                      (try (Integer/parseInt int-str)
                                                                                           (catch Exception _ :no-result)))
                                                                                    int-parse-strings)
                                                                               (repeat :no-result)))]
                                     (apply +' (map (fn [[cor-int res-int]]
                                                      (if (not (number? res-int))
                                                        ;; penalty for not enough lines with parseable integers
                                                        100
                                                        (abs (- cor-int res-int))))
                                                    correct-result-int-pairs))))]}

      "for-loop-index" {:input->type {'input1 {:type 'int?}
                                      'input2 {:type 'int?}
                                      'input3 {:type 'int?}}
                        :ret-type {:type 'string?}
                        :other-type-ctors #{'int? 'boolean?}
                        :extra-genes [{:gene :lit
                                       :val "\n"
                                       :type lib/STRING}
                                      {:gene :lit
                                       :val \newline
                                       :type lib/CHAR}
                                      {:gene :fn
                                       :arg-types [lib/INT]
                                       :ret-type lib/BOOLEAN}]
                        :loss-fns [lev/distance]}

      "grade" {:input->type {'input1 {:type 'int?}
                             'input2 {:type 'int?}
                             'input3 {:type 'int?}
                             'input4 {:type 'int?}
                             'input5 {:type 'int?}}
               :ret-type {:type 'string?}
               :other-type-ctors #{'boolean?}
               :extra-genes [{:gene :lit
                              :val "Student has a "
                              :type {:type 'string?}}
                             {:gene :lit
                              :val " grade."
                              :type {:type 'string?}}
                             {:gene :lit
                              :val "A"
                              :type {:type 'string?}}
                             {:gene :lit
                              :val "B"
                              :type {:type 'string?}}
                             {:gene :lit
                              :val "C"
                              :type {:type 'string?}}
                             {:gene :lit
                              :val "D"
                              :type {:type 'string?}}
                             {:gene :lit
                              :val "F"
                              :type {:type 'string?}}
                             {:gene :lit-generator
                              :fn #(rand-int 101)
                              :type {:type 'int?}}]
               :loss-fns [lev/distance
                          ;; If correct format, distance from correct letter grade char.
                          (let [extract-letter #(second (re-find #"^Student has a (.) grade.$" %))]
                            #(let [actual-letter (extract-letter %1)
                                   expected-letter (extract-letter %2)]
                               (if actual-letter
                                 (Math/abs (- (int (first expected-letter))
                                              (int (first actual-letter))))
                                 penalty)))]}

      "last-index-of-zero" {:input->type {'input1 {:type :vector
                                                   :child {:type 'int?}}}
                            :ret-type {:type 'int?}
                            :other-type-ctors #{'boolean?}
                            :extra-genes [{:gene :lit-generator
                                           :fn (bu/int-generator 50)
                                           :type {:type 'int?}}
                                          {:gene :fn
                                           :arg-types [lib/INT]
                                           :ret-type lib/BOOLEAN}]
                            :loss-fns [bu/absolute-distance]}

      "median" {:input->type {'input1 {:type 'int?}
                              'input2 {:type 'int?}
                              'input3 {:type 'int?}}
                :ret-type {:type 'int?}
                :other-type-ctors #{'boolean?}
                :extra-genes [{:gene :lit-generator
                               :fn (bu/int-generator 100)
                               :type {:type 'int?}}]
                :loss-fns [bu/absolute-distance]}

      "mirror-image" {:input->type {'input1 {:type :vector
                                             :child {:type 'int?}}
                                    'input2 {:type :vector
                                             :child {:type 'int?}}}
                      :ret-type {:type 'boolean?}
                      :other-type-ctors #{'int?}
                      :extra-genes [{:gene :lit-generator
                                     :fn bu/rand-bool
                                     :type {:type 'boolean?}}
                                    {:gene :fn
                                     :arg-types [lib/INT]
                                     :ret-type lib/BOOLEAN}]
                      :loss-fns [#(if (= %1 %2) 0 1)]
                      :solution (list {:gene :local
                                       :idx 0}
                                      {:gene :local
                                       :idx 1}
                                      {:gene :var
                                       :name `lib/reversev}
                                      {:gene :apply}
                                      {:gene :var
                                       :name '=}
                                      {:gene :apply})}

      "negative-to-zero" {:input->type {'input1 {:type :vector
                                                 :child {:type 'int?}}}
                          :ret-type {:type :vector
                                     :child {:type 'int?}}
                          :other-type-ctors #{'int? 'boolean?}
                          :extra-genes [{:gene :lit
                                         :val 0
                                         :type {:type 'int?}}
                                        {:gene :fn
                                         :arg-types [lib/INT]
                                         :ret-type lib/INT}
                                        {:gene :fn
                                         :arg-types [lib/INT]
                                         :ret-type lib/BOOLEAN}]
                          :loss-fns [lev/distance]}

      "number-io" {:input->type {'input1 {:type 'double?}
                                 'input2 {:type 'int?}}
                   :ret-type {:type 'string?}
                   :extra-genes [{:gene :lit-generator
                                  :fn (bu/int-generator 100)
                                  :type {:type 'int?}}
                                 {:gene :lit-generator
                                  :fn #(- (rand 201.0) 100.0)
                                  :type {:type 'double?}}]
                   :loss-fns [#(try
                                 (bu/round 4 (Math/abs (- (Double/parseDouble %1) %2)))
                                 (catch Exception _ penalty))
                              #(lev/distance (take 10 %1)
                                             (take 10 (pr-str %2)))]
                   :solution (list {:gene :local
                                    :idx 0}
                                   {:gene :local
                                    :idx 1}
                                   {:gene :var
                                    :name 'double}
                                   {:gene :apply}
                                   {:gene :var
                                    :name 'double-add}
                                   {:gene :apply}
                                   {:gene :var
                                    :name 'str}
                                   {:gene :apply})}

      "pig-latin" {:input->type {'input1 {:type 'string?}}
                   :ret-type {:type 'string?}
                   :other-type-ctors #{'int? 'boolean? 'char?}
                   :extra-genes [{:gene :lit
                                  :val \a
                                  :type {:type 'char?}}
                                 {:gene :lit
                                  :val \e
                                  :type {:type 'char?}}
                                 {:gene :lit
                                  :val \i
                                  :type {:type 'char?}}
                                 {:gene :lit
                                  :val \o
                                  :type {:type 'char?}}
                                 {:gene :lit
                                  :val \u
                                  :type {:type 'char?}}
                                 {:gene :lit
                                  :val \space
                                  :type {:type 'char?}}
                                 {:gene :lit
                                  :val "ay"
                                  :type {:type 'string?}}
                                 {:gene :lit
                                  :val "aeiou"
                                  :type {:type 'string?}}
                                 {:gene :lit-generator
                                  :fn bu/rand-char
                                  :type {:type 'char}}
                                 {:gene :lit-generator
                                  :fn (bu/string-generator 21)
                                  :type {:type 'string?}}
                                 {:gene :fn
                                  :arg-types [lib/STRING]
                                  :ret-type lib/STRING}]
                   :loss-fns [lev/distance]}

      "replace-space-with-newline" {:input->type {'input1 {:type 'string?}}
                                    :ret-type {:type 'int?}
                                    ;; The `nil?` functions are side effects, like printing.
                                    :other-type-ctors #{'boolean? 'char? 'nil?}
                                    :extra-genes [{:gene :lit
                                                   :val \space
                                                   :type {:type 'char?}}
                                                  {:gene :lit
                                                   :val \newline
                                                   :type {:type 'char?}}
                                                  {:gene :lit-generator
                                                   :fn bu/rand-char
                                                   :type {:type 'char?}}
                                                  {:gene :fn
                                                   :arg-types [lib/CHAR]
                                                   :ret-type lib/CHAR}
                                                  {:gene :fn
                                                   :arg-types [lib/STRING]
                                                   :ret-type lib/STRING}
                                                  {:gene :fn
                                                   :arg-types [lib/CHAR]
                                                   :ret-type lib/INT}
                                                  {:gene :fn
                                                   :arg-types [lib/STRING]
                                                   :ret-type lib/INT}]
                                    :loss-fns [bu/absolute-distance]
                                     ;; Config for how to unpack the cases from data files.
                                    :out-key :output2
                                    :stdout-key :output1
                                    :solution (list {:gene :lit
                                                     :val \newline
                                                     :type {:type 'char?}}
                                                    {:gene :lit
                                                     :val \space
                                                     :type {:type 'char?}}
                                                    {:gene :local
                                                     :idx 0}
                                                    {:gene :var
                                                     :name `lib/replace-char}
                                                    {:gene :apply}
                                                    {:gene :let}
                                                    ;; Return
                                                    {:gene :lit
                                                     :val \newline
                                                     :type {:type 'char?}}
                                                    {:gene :local
                                                     :idx 1}
                                                    {:gene :var
                                                     :name `lib/remove-char}
                                                    {:gene :apply}
                                                    {:gene :var
                                                     :name 'length}
                                                    {:gene :apply}
                                                    ;; Print
                                                    {:gene :local
                                                     :idx 1}
                                                    {:gene :var
                                                     :name 'print}
                                                    {:gene :apply}
                                                    ;; Wrap 2 expressions in do
                                                    {:gene :var
                                                     :name 'do2}
                                                    {:gene :apply})}

      "scrabble-score" {:input->type {'input1 {:type 'string?}}
                        :ret-type {:type 'int?}
                        :other-type-ctors #{'boolean? 'char?}
                        :extra-genes [{:gene :lit
                                       :val (let [scrabble-map {\a 1, \b 3, \c 3, \d 2, \e 1, \f 4, \g 2, \h 4, \i 1,
                                                                \j 8, \k 5, \l 1, \m 3, \n 1, \o 1, \p 3, \q 10, \r 1,
                                                                \s 1, \t 1, \u 1, \v 4, \w 4, \x 8, \y 4, \z 10}
                                                  visible-chars (map char (range 0 127))]
                                              (vec (for [c visible-chars]
                                                     (get scrabble-map (first (str/lower-case c)) 0))))
                                       :type {:type :vector
                                              :child {:type 'int?}}}
                                      {:gene :fn
                                       :arg-types [lib/STRING]
                                       :ret-type lib/INT}
                                      {:gene :fn
                                       :arg-types [lib/CHAR]
                                       :ret-type lib/INT}]
                        :loss-fns [bu/absolute-distance]}

      "small-or-large" {:input->type {'input1 {:type 'int?}}
                        :ret-type {:type 'string?}
                        :other-type-ctors #{'boolean?}
                        :extra-genes [{:gene :lit
                                       :val "small"
                                       :type {:type 'string?}}
                                      {:gene :lit
                                       :val "large"
                                       :type {:type 'string?}}
                                      {:gene :lit-generator
                                       :fn (bu/int-generator 10000)
                                       :type {:type 'int?}}]
                        :loss-fns [lev/distance]}

      "smallest" {:input->type {'input1 {:type 'int?}
                                'input2 {:type 'int?}
                                'input3 {:type 'int?}
                                'input4 {:type 'int?}}
                  :ret-type {:type 'int?}
                  :other-type-ctors #{'boolean?}
                  :extra-genes [{:gene :lit-generator
                                 :fn (bu/int-generator 100)
                                 :type {:type 'int?}}]
                  :loss-fns [bu/absolute-distance]
                  :solution [{:gene :local
                              :idx 0}
                             {:gene :local
                              :idx 1}
                             {:gene :var
                              :name `lib/min'}
                             {:gene :apply}
                             {:gene :local
                              :idx 2}
                             {:gene :var
                              :name `lib/min'}
                             {:gene :apply}
                             {:gene :local
                              :idx 3}
                             {:gene :var
                              :name `lib/min'}
                             {:gene :apply}]}

      "string-differences" {:input->type {'input1 {:type 'string?}
                                          'input2 {:type 'string?}}
                            :ret-type {:type 'string?}
                            :other-type-ctors #{'int? 'boolean? 'char?}
                            :extra-genes [{:gene :lit
                                           :val \space
                                           :type {:type 'char?}}
                                          {:gene :lit
                                           :val \newline
                                           :type {:type 'char?}}
                                          {:gene :lit-generator
                                           :fn (bu/int-generator 10)
                                           :type {:type 'int?}}
                                          {:gene :fn
                                           :arg-types [lib/STRING lib/STRING]
                                           :ret-type lib/BOOLEAN}
                                          {:gene :fn
                                           :arg-types [lib/CHAR lib/CHAR]
                                           :ret-type lib/BOOLEAN}]
                            :loss-fns [;; 1. Levenshtein distance of printed strings
                                       lev/distance
                                       ;; 2. Difference in number of lines using the correct format
                                       (fn [result correct-output]
                                         (abs (- (count (re-seq #"(?m)^\d+ \S \S$" correct-output))
                                                 (count (re-seq #"(?m)^\d+ \S \S$" result)))))]}

      "string-lengths-backwards" {:input->type {'input1 {:type :vector
                                                         :child {:type 'string?}}}
                                  :ret-type {:type 'string?}
                                  :other-type-ctors #{'int? 'boolean?}
                                  :extra-genes [{:gene :lit-generator
                                                 :fn (bu/int-generator 100)
                                                 :type {:type 'int?}}
                                                {:gene :fn
                                                 :arg-types [lib/STRING]
                                                 :ret-type lib/INT}]
                                  :loss-fns [lev/distance]}

      "sum-of-squares" {:input->type {'input1 {:type 'int?}}
                        :ret-type {:type 'int?}
                        :other-type-ctors #{'boolean?}
                        :extra-genes [{:gene :lit
                                       :val 0
                                       :type {:type 'int?}}
                                      {:gene :lit
                                       :val 1
                                       :type {:type 'int?}}
                                      {:gene :lit-generator
                                       :fn (bu/int-generator 100)
                                       :type {:type 'int?}}
                                      {:gene :fn
                                       :arg-types [lib/INT]
                                       :ret-type lib/INT}]
                        :loss-fns [bu/absolute-distance]}

      "super-anagrams" {:input->type {'input1 {:type 'string?}
                                      'input2 {:type 'string?}}
                        :ret-type {:type 'boolean?}
                        :other-type-ctors #{'int? 'char?}
                        :extra-genes [{:gene :lit-generator
                                       :fn bu/rand-bool
                                       :type {:type 'boolean?}}
                                      {:gene :lit-generator
                                       :fn (bu/int-generator 1000)
                                       :type {:type 'int?}}
                                      {:gene :lit-generator
                                       :fn bu/rand-char
                                       :type {:type 'char?}}
                                      {:gene :fn
                                       :arg-types [lib/STRING]
                                       :ret-type lib/INT}
                                      {:gene :fn
                                       :arg-types [lib/CHAR]
                                       :ret-type lib/INT}]
                        :loss-fns [#(if (= %1 %2) 0 1)]}

      "syllables" {:input->type {'input1 {:type 'string?}}
                   :ret-type {:type 'string?}
                   :other-type-ctors #{'int? 'boolean? 'char?}
                   :extra-genes [{:gene :lit
                                  :val "The number of syllables is "
                                  :type {:type 'string?}}
                                 {:gene :lit
                                  :val \a
                                  :type {:type 'char?}}
                                 {:gene :lit
                                  :val \e
                                  :type {:type 'char?}}
                                 {:gene :lit
                                  :val \i
                                  :type {:type 'char?}}
                                 {:gene :lit
                                  :val \o
                                  :type {:type 'char?}}
                                 {:gene :lit
                                  :val \u
                                  :type {:type 'char?}}
                                 {:gene :lit
                                  :val \y
                                  :type {:type 'char?}}
                                 {:gene :lit
                                  :val "aeiouy"
                                  :type {:type 'string?}}
                                 {:gene :lit-generator
                                  :fn bu/rand-char
                                  :type {:type 'char?}}
                                 {:gene :fn
                                  :arg-types [lib/CHAR]
                                  :ret-type lib/INT}
                                 {:gene :fn
                                  :arg-types [lib/CHAR]
                                  :ret-type lib/BOOLEAN}
                                 {:gene :fn
                                  :arg-types [lib/STRING]
                                  :ret-type lib/INT}
                                 {:gene :fn
                                  :arg-types [lib/STRING]
                                  :ret-type lib/BOOLEAN}]
                   :loss-fns [lev/distance
                              (let [parse #(try (Integer/parseInt (last (str/split % #"\s+")))
                                                (catch Exception _ nil))]
                                #(if-let [num (parse %1)]
                                   (bu/absolute-distance num (parse %2))
                                   penalty))]}

      "vector-average" {:input->type {'input1 {:type :vector
                                               :child {:type 'double?}}}
                        :ret-type {:type 'double?}
                        :other-type-ctors #{'int?}
                        :extra-genes [{:gene :fn
                                       :arg-types [lib/DOUBLE lib/DOUBLE]
                                       :ret-type lib/DOUBLE}]
                        :loss-fns [#(bu/round 4 (bu/absolute-distance %1 %2))]}

      "vectors-summed" {:input->type {'input1 {:type :vector
                                               :child {:type 'int?}}
                                      'input2 {:type :vector
                                               :child {:type 'int?}}}
                        :ret-type {:type :vector
                                   :child {:type 'int?}}
                        :extra-genes [{:gene :lit
                                       :val []
                                       :type {:type :vector
                                              :child {:type 'int?}}}
                                      {:gene :lit-generator
                                       :fn (bu/int-generator 1000)
                                       :type {:type 'int?}}
                                      {:gene :fn
                                       :arg-types [lib/INT, lib/INT]
                                       :ret-type lib/INT}]
                        :loss-fns [(fn [y-hat y]
                                     (reduce +' (map #(or (bu/absolute-distance %1 %2) penalty)
                                                     y-hat y)))
                                   (fn [y-hat y]
                                     (*' 1000 (bu/absolute-distance (count y-hat) (count y))))]
                        :solution (list {:gene :local
                                         :idx 0}
                                        {:gene :local
                                         :idx 1}
                                        {:gene :var
                                         :name 'int-add}
                                        {:gene :var
                                         :name 'map2-vec}
                                        {:gene :apply})}

   ; "wallis-pi"
   ; "word-stats"

      "x-word-lines" {:input->type {'input1 {:type 'string?}
                                    'input2 {:type 'int?}}
                      :ret-type {:type 'string?}
                      :other-type-ctors [{:type 'boolean?} {:type 'char?}]
                      ;; @todo What anonymous function signatures should x-word-lines support?
                      :extra-genes [{:gene :lit
                                     :val \newline
                                     :type {:type 'char?}}
                                    {:gene :lit
                                     :val \space
                                     :type {:type 'char?}}]
                      :loss-fns [;; First error is Levenshtein distance of printed strings
                                 lev/distance
                                 ;; Second error is integer distance from the correct number of newlines
                                 (fn [result correct-output]
                                   (abs (- (count (filter #(= % \newline) correct-output))
                                           (count (filter #(= % \newline) result)))))
                                 ;; Third error is summed error of integer distances over the lines of the correct number of words per line
                                 (fn [result correct-output]
                                   (let [result-split-lines (str/split-lines result)
                                         words-per-line (if (empty? result-split-lines)
                                                          0
                                                          (count (str/split (first result-split-lines)
                                                                            #"\s+")))]
                                     (+ (apply + (map #(abs (- words-per-line
                                                               (count (str/split (str/trim %) #"\s+"))))
                                                      (butlast result-split-lines)))
                                        (abs (- (count (str/split (str/trim (let [last-line (last (str/split-lines correct-output))]
                                                                              (if last-line last-line "")))
                                                                  #"\s+"))
                                                (count (str/split (str/trim (let [last-line (last result-split-lines)]
                                                                              (if last-line last-line "")))
                                                                  #"\s+")))))))]}

      ;;;;;;;;;;;;;;;;;;;
      ;; PSB2 Problems ;;
      ;;;;;;;;;;;;;;;;;;;

      "basement" {:input->type {'input1 {:type :vector
                                         :child {:type 'int?}}}
                  :ret-type {:type 'int?}
                  :other-type-ctors [{:type 'boolean?}]
                  :extra-genes [{:gene :lit
                                 :val []
                                 :type {:type :vector
                                        :child {:type 'int?}}}
                                {:gene :lit
                                 :val -1
                                 :type {:type 'int?}}
                                {:gene :lit
                                 :val 0
                                 :type {:type 'int?}}
                                {:gene :lit
                                 :val 1
                                 :type {:type 'int?}}
                                {:gene :lit-generator
                                 :fn (bu/int-generator 1000)
                                 :type {:type 'int?}}]
                  :loss-fns [bu/absolute-distance]}

      "bouncing-balls" {:input->type {'input1 {:type 'double?}
                                      'input2 {:type 'double?}
                                      'input3 {:type 'int?}}
                        :ret-type {:type 'double?}
                        :other-type-ctors [{:type 'boolean?}]
                        :extra-genes [{:gene :lit
                                       :val 0.0
                                       :type {:type 'double?}}
                                      {:gene :lit
                                       :val 1.0
                                       :type {:type 'double?}}
                                      {:gene :lit
                                       :val 2.0
                                       :type {:type 'double?}}]
                        :loss-fns [#(bu/round 3 (bu/absolute-distance %1 %2))]}

      "bowling" {:input->type {'input1 {:type 'string?}}
                 :ret-type {:type 'int?}
                 :other-type-ctors [{:type 'boolean?} {:type 'char?}]
                 :extra-genes [{:gene :lit
                                :val \-
                                :type {:type 'char?}}
                               {:gene :lit
                                :val \X
                                :type {:type 'char?}}
                               {:gene :lit
                                :val \/
                                :type {:type 'char?}}
                               {:gene :lit
                                :val \1
                                :type {:type 'char?}}
                               {:gene :lit
                                :val \2
                                :type {:type 'char?}}
                               {:gene :lit
                                :val \3
                                :type {:type 'char?}}
                               {:gene :lit
                                :val \4
                                :type {:type 'char?}}
                               {:gene :lit
                                :val \5
                                :type {:type 'char?}}
                               {:gene :lit
                                :val \6
                                :type {:type 'char?}}
                               {:gene :lit
                                :val \7
                                :type {:type 'char?}}
                               {:gene :lit
                                :val \8
                                :type {:type 'char?}}
                               {:gene :lit
                                :val \9
                                :type {:type 'char?}}
                               {:gene :lit
                                :val 10
                                :type {:type 'int?}}
                               {:gene :lit-generator
                                :fn (bu/int-generator 10)
                                :type {:type 'int?}}]
                 :loss-fns [bu/absolute-distance]}

      "camel-case" {:input->type {'input1 {:type 'string?}}
                    :ret-type {:type 'string?}
                    :other-type-ctors [{:type 'int?} {:type 'boolean?} {:type 'char?}]
                    :extra-genes [{:gene :lit
                                   :val \-
                                   :type {:type 'char?}}
                                  {:gene :lit
                                   :val \space
                                   :type {:type 'char?}}
                                  {:gene :lit-generator
                                   :fn bu/rand-char
                                   :type {:type 'char?}}
                                  {:gene :lit-generator
                                   :fn (bu/string-generator 21)
                                   :type {:type 'string?}}]
                    :loss-fns [lev/distance]}

;;  "coin-sums" ;; NEEDS MULTIPLE OUTPUTS

      "cut-vector" {:input->type {'input1 {:type :vector
                                           :child {:type 'int?}}}
                    :ret-type {:type :tuple
                               :children [{:type :vector
                                           :child {:type 'int?}}
                                          {:type :vector
                                           :child {:type 'int?}}]}
                    :out-key [:output1 :output2]
                    :other-type-ctors [{:type 'int?} {:type 'boolean?}]
                    :extra-genes [{:gene :lit
                                   :val 0
                                   :type {:type 'int?}}
                                  {:gene :lit
                                   :val []
                                   :type {:type :vector
                                          :child {:type 'int?}}}
                                  ;; This is a random vector generator
                                  {:gene :lit-generator
                                   :fn (fn [] (vec (repeatedly (rand-int 21) #(inc (rand-int 10000)))))
                                   :type {:type :vector
                                          :child {:type 'int?}}}]
                    :loss-fns [#(bu/vector-of-numbers-loss (first %1) (first %2))
                               #(bu/vector-of-numbers-loss (second %1) (second %2))]}

      "dice-game" {:input->type {'input1 {:type 'int?}
                                 'input2 {:type 'int?}}
                   :ret-type {:type 'double?}
                   :other-type-ctors [{:type 'boolean?}]
                   :extra-genes [{:gene :lit
                                  :val 0.0
                                  :type {:type 'double?}}
                                 {:gene :lit
                                  :val 1.0
                                  :type {:type 'double?}}]
                   :loss-fns [#(bu/round 3 (bu/absolute-distance %1 %2))]}

      "find-pair" {:input->type {'input1 {:type :vector
                                          :child {:type 'int?}}
                                 'input2 {:type 'int?}}
                   :ret-type {:type :tuple
                              :children [{:type 'int?} {:type 'int?}]}
                   :out-key [:output1 :output2]
                   :other-type-ctors [{:type 'boolean?}]
                   :extra-genes [{:gene :lit
                                  :val -1
                                  :type {:type 'int?}}
                                 {:gene :lit
                                  :val 0
                                  :type {:type 'int?}}
                                 {:gene :lit
                                  :val 1
                                  :type {:type 'int?}}
                                 {:gene :lit
                                  :val 2
                                  :type {:type 'int?}}
                                 {:gene :lit-generator
                                  :fn (bu/int-generator 1000)
                                  :type {:type 'int?}}]
                   :loss-fns [#(bu/absolute-distance (first %1) (first %2))
                              #(bu/absolute-distance (second %1) (second %2))]}

      "fizz-buzz" {:input->type {'input1 {:type 'int?}}
                   :ret-type {:type 'string?}
                   :other-type-ctors [{:type 'boolean?}]
                   :extra-genes [{:gene :lit
                                  :val "Fizz"
                                  :type {:type 'string?}}
                                 {:gene :lit
                                  :val "Buzz"
                                  :type {:type 'string?}}
                                 {:gene :lit
                                  :val "FizzBuzz"
                                  :type {:type 'string?}}
                                 {:gene :lit
                                  :val 0
                                  :type {:type 'int?}}
                                 {:gene :lit
                                  :val 3
                                  :type {:type 'int?}}
                                 {:gene :lit
                                  :val 5
                                  :type {:type 'int?}}]
                   :loss-fns [lev/distance]}

      "fuel-cost" {:input->type {'input1 {:type :vector
                                          :child {:type 'int?}}}
                   :ret-type {:type 'int?}
                   :other-type-ctors [{:type 'boolean?}]
                   :extra-genes [{:gene :lit-generator
                                  :fn (bu/int-generator 1000)
                                  :type {:type 'int?}}
                                 {:gene :lit
                                  :val 0
                                  :type {:type 'int?}}
                                 {:gene :lit
                                  :val 1
                                  :type {:type 'int?}}
                                 {:gene :lit
                                  :val 2
                                  :type {:type 'int?}}
                                 {:gene :lit
                                  :val 3
                                  :type {:type 'int?}}]
                   :loss-fns [bu/absolute-distance]
                   :solution (list {:gene :local
                                    :idx 0}
                                   ;; Anonymous function
                                   {:gene :fn
                                    :arg-types [lib/INT]
                                    :ret-type lib/INT}
                                   {:gene :lit
                                    :val 2
                                    :type {:type 'int?}}
                                   {:gene :lit
                                    :val 3
                                    :type {:type 'int?}}
                                   {:gene :local
                                    :idx 1}
                                   {:gene :var
                                    :name 'int-quot}
                                   {:gene :apply}
                                   {:gene :var
                                    :name 'int-sub}
                                   {:gene :apply}
                                   {:gene :close}
                                   ;; Map fn over input vector
                                   {:gene :var
                                    :name 'map-vec}
                                   {:gene :apply}
                                   ;; Sum the vector
                                   {:gene :var
                                    :name 'int-add}
                                   {:gene :var
                                    :name 'reduce-vec}
                                   {:gene :apply})}

      "gcd" {:input->type {'input1 {:type 'int?}
                           'input2 {:type 'int?}}
             :ret-type {:type 'int?}
             :other-type-ctors [{:type 'boolean?}]
             :extra-genes [{:gene :lit-generator
                            :fn (bu/int-generator 10)
                            :type {:type 'int?}}]
             :loss-fns [bu/absolute-distance]}

      "indices-of-substring" {:input->type {'input1 {:type 'string?}
                                            'input2 {:type 'string?}}
                              :ret-type {:type :vector
                                         :child {:type 'int?}}
                              :other-type-ctors [{:type 'int?} {:type 'boolean?} {:type 'char?}]
                              :extra-genes [{:gene :lit
                                             :val []
                                             :type {:type :vector
                                                    :child {:type 'int?}}}
                                            {:gene :lit
                                             :val ""
                                             :type {:type 'string?}}
                                            {:gene :lit
                                             :val 0
                                             :type {:type 'int?}}
                                            {:gene :lit
                                             :val 1
                                             :type {:type 'int?}}]
                              ;; Note: this error function of lev/distance is correct for Indices of Substring
                              :loss-fns [lev/distance]}

      "leaders" {:input->type {'input1 {:type :vector
                                        :child {:type 'int?}}}
                 :ret-type {:type :vector
                            :child {:type 'int?}}
                 :other-type-ctors [{:type 'int?} {:type 'boolean?}]
                 :extra-genes [{:gene :lit
                                :val []
                                :type {:type :vector
                                       :child {:type 'int?}}}
                               ;; This is a random vector generator
                               {:gene :lit-generator
                                :fn (fn [] (vec (repeatedly (rand-int 21) #(rand-int 1001))))
                                :type {:type :vector
                                       :child {:type 'int?}}}]
                 :loss-fns [bu/vector-of-numbers-loss]}

      "luhn" {:input->type {'input1 {:type :vector
                                     :child {:type 'int?}}}
              :ret-type {:type 'int?}
              :other-type-ctors [{:type 'boolean?}]
              :extra-genes [{:gene :lit
                             :val 0
                             :type {:type 'int?}}
                            {:gene :lit
                             :val 2
                             :type {:type 'int?}}
                            {:gene :lit
                             :val 9
                             :type {:type 'int?}}
                            {:gene :lit
                             :val 10
                             :type {:type 'int?}}
                            {:gene :lit-generator
                             :fn (bu/int-generator 10)
                             :type {:type 'int?}}]
              :loss-fns [bu/absolute-distance]}

   ;;  "mastermind" ;; NEEDS MULTIPLE OUTPUTS

      "middle-character" {:input->type {'input1 {:type 'string?}}
                          :ret-type {:type 'string?}
                          :other-type-ctors [{:type 'int?} {:type 'boolean?} {:type 'char?}]
                          :extra-genes [{:gene :lit
                                         :val ""
                                         :type {:type 'string?}}
                                        {:gene :lit
                                         :val 0
                                         :type {:type 'int?}}
                                        {:gene :lit
                                         :val 1
                                         :type {:type 'int?}}
                                        {:gene :lit
                                         :val 2
                                         :type {:type 'int?}}
                                        {:gene :lit-generator
                                         :fn (bu/int-generator 100)
                                         :type {:type 'int?}}]
                          :loss-fns [lev/distance]}

      "paired-digits" {:input->type {'input1 {:type 'string?}}
                       :ret-type {:type 'int?}
                       :other-type-ctors [{:type 'boolean?} {:type 'char?}]
                       :extra-genes [{:gene :lit
                                      :val 0
                                      :type {:type 'int?}}
                                     {:gene :lit-generator
                                      :fn (fn [] (rand-nth "0123456789"))
                                      :type {:type 'char?}}
                                     {:gene :lit-generator
                                      :fn (fn [] (rand-int 10))
                                      :type {:type 'int?}}]
                       :loss-fns [bu/absolute-distance]}

      "shopping-list" {:input->type {'input1 {:type :vector
                                              :child {:type 'double?}}
                                     'input2 {:type :vector
                                              :child {:type 'double?}}}
                       :ret-type {:type 'double?}
                       :other-type-ctors [{:type 'boolean?} {:type 'int?}]
                       :extra-genes [{:gene :lit
                                      :val 0.0
                                      :type {:type 'double?}}
                                     {:gene :lit
                                      :val 100.0
                                      :type {:type 'double?}}
                                     {:gene :lit-generator
                                      :fn (fn [] (* (rand) 100))
                                      :type {:type 'double?}}]
                       :loss-fns [#(bu/round 2 (bu/absolute-distance %1 %2))]}

      "snow-day" {:input->type {'input1 {:type 'int?}
                                'input2 {:type 'double?}
                                'input3 {:type 'double?}
                                'input4 {:type 'double?}}
                  :ret-type {:type 'double?}
                  :other-type-ctors [{:type 'boolean?}]
                  :extra-genes [{:gene :lit
                                 :val 0
                                 :type {:type 'int?}}
                                {:gene :lit
                                 :val 1
                                 :type {:type 'int?}}
                                {:gene :lit
                                 :val -1
                                 :type {:type 'int?}}
                                {:gene :lit
                                 :val 0.0
                                 :type {:type 'double?}}
                                {:gene :lit
                                 :val 1.0
                                 :type {:type 'double?}}
                                {:gene :lit
                                 :val -1.0
                                 :type {:type 'double?}}]
                  :loss-fns [#(bu/round 3 (bu/absolute-distance %1 %2))]}

      "solve-boolean" {:input->type {'input1 {:type 'string?}}
                       :ret-type {:type 'boolean?}
                       :other-type-ctors [{:type 'int?} {:type 'char?}]
                       :extra-genes [{:gene :lit
                                      :val true
                                      :type {:type 'boolean?}}
                                     {:gene :lit
                                      :val false
                                      :type {:type 'boolean?}}
                                     {:gene :lit
                                      :val \t
                                      :type {:type 'char?}}
                                     {:gene :lit
                                      :val \f
                                      :type {:type 'char?}}
                                     {:gene :lit
                                      :val \|
                                      :type {:type 'char?}}
                                     {:gene :lit
                                      :val \&
                                      :type {:type 'char?}}]
                       :loss-fns [#(if (= %1 %2) 0 1)]}

      "spin-words" {:input->type {'input1 {:type 'string?}}
                    :ret-type {:type 'string?}
                    :other-type-ctors [{:type 'int?} {:type 'char?} {:type 'boolean?}]
                    :extra-genes [{:gene :lit
                                   :val 4
                                   :type {:type 'int?}}
                                  {:gene :lit
                                   :val 5
                                   :type {:type 'int?}}
                                  {:gene :lit
                                   :val \space
                                   :type {:type 'char?}}
                                  {:gene :lit-generator
                                   :fn bu/rand-char
                                   :type {:type 'char?}}
                                  {:gene :lit-generator
                                   :fn (bu/string-generator 21)
                                   :type {:type 'string?}}]
                    :loss-fns [lev/distance]}

      "square-digits" {:input->type {'input1 {:type 'int?}}
                       :ret-type {:type 'string?}
                       :other-type-ctors [{:type 'char?} {:type 'boolean?}]
                       :extra-genes [{:gene :lit
                                      :val ""
                                      :type {:type 'string?}}
                                     {:gene :lit
                                      :val 0
                                      :type {:type 'int?}}
                                     {:gene :lit
                                      :val 1
                                      :type {:type 'int?}}
                                     {:gene :lit
                                      :val 2
                                      :type {:type 'int?}}
                                     {:gene :lit-generator
                                      :fn (bu/int-generator 100)
                                      :type {:type 'int?}}]
                       :loss-fns [lev/distance]}

      "substitution-cipher" {:input->type {'input1 {:type 'string?}
                                           'input2 {:type 'string?}
                                           'input3 {:type 'string?}}
                             :ret-type {:type 'string?}
                             :other-type-ctors [{:type 'int?} {:type 'char?} {:type 'boolean?}]
                             :extra-genes [{:gene :lit
                                            :val ""
                                            :type {:type 'string?}}
                                           {:gene :lit
                                            :val 0
                                            :type {:type 'int?}}]
                             :loss-fns [lev/distance]}

      "twitter" {:input->type {'input1 {:type 'string?}}
                 :ret-type {:type 'string?}
                 :other-type-ctors [{:type 'int?} {:type 'char?} {:type 'boolean?}]
                 :extra-genes [{:gene :lit
                                :val 0
                                :type {:type 'int?}}
                               {:gene :lit
                                :val 140
                                :type {:type 'int?}}
                               {:gene :lit
                                :val "Too many characters"
                                :type {:type 'string?}}
                               {:gene :lit
                                :val "You didn't type anything"
                                :type {:type 'string?}}
                               {:gene :lit
                                :val "Your tweet has "
                                :type {:type 'string?}}
                               {:gene :lit
                                :val " characters"
                                :type {:type 'string?}}]
                 :loss-fns [lev/distance]}

      "vector-distance" {:input->type {'input1 {:type :vector
                                                :child {:type 'double?}}
                                       'input2 {:type :vector
                                                :child {:type 'double?}}}
                         :ret-type {:type 'double?}
                         :other-type-ctors [{:type 'boolean?} {:type 'int?}]
                         :extra-genes [{:gene :lit
                                        :val []
                                        :type {:type :vector
                                               :child {:type 'double?}}}
                                       {:gene :lit
                                        :val 0
                                        :type {:type 'int?}}]
                         :loss-fns [#(bu/round 3 (bu/absolute-distance %1 %2))]}}
     ;; Add nil penalties to all loss functions.
     (fn [problem-map]
       (update problem-map :loss-fns #(map penalize-nil %))))))

(defn reshape-case
  [case {:keys [out-key stdout-key]
         :or   {out-key :output1}}]
  (merge
   {:inputs (->> case
                 (filter (fn [[k _]] (str/starts-with? (name k) "input")))
                 (sort-by first)
                 (mapv second))
    :output (if (sequential? out-key)
              (vec (map #(get case %) out-key))
              (out-key case))}
   (when stdout-key
     {:std-out (stdout-key case)})))

(defn read-cases
  [{:keys [data-dir problem n-train n-test]}]
  (let [problem-info         (get (problems {}) (name problem))
        reshape              #(reshape-case % problem-info)
        {:keys [train test]} (psb2/fetch-examples (str data-dir) (str problem) n-train n-test)]
    {:train (map reshape train)
     :test  (map reshape test)}))

(defn validate-solutions
  [{:keys [data-dir num-cases]}]
  (let [suite (problems {:penalty 1000})]
    (doseq [[problem-name task] (filter (fn [[_ task]] (contains? task :solution)) suite)]
      (println "Starting" problem-name)
      (let [factory    (i/make-evaluator (-> task
                                             task/enhance-task
                                             (assoc :evaluate-fn i/evaluate-full-behavior
                                                    :cases (:test (read-cases {:data-dir (name data-dir)
                                                                               :problem  problem-name
                                                                               :n-test   num-cases
                                                                               :n-train  0})))))
            start-time (System/currentTimeMillis)
            evaluation (factory (:solution task) nil)
            duration   (/ (- (System/currentTimeMillis) start-time) 1000.0)]
        (cond
          (> (:total-error evaluation) 0)
          (throw (ex-info (str problem-name " solution has non-zero error.") {:eval evaluation}))

          (some? (:exception evaluation))
          (throw (ex-info (str problem-name " solution threw an error.") {:eval evaluation} (:exception evaluation)))

          :else
          (println problem-name "passed in" duration "seconds."))))))

(comment

  (validate-solutions {:data-dir  "data/psb/"
                       :num-cases 50}))
