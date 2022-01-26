(ns erp12.cbgp-lite.benchmark.psb
  (:gen-class)
  (:require [erp12.ga-clj.toolbox :as tb]
            [erp12.cbgp-lite.gp :as gp]
            [erp12.cbgp-lite.benchmark.psb-utils :as pu]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.compile :as c]
            [taoensso.timbre :as log]
            [clj-fuzzy.levenshtein :as lev]
            [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(def config
  {:n-train              100
   :n-test               300
   :population-size      1000
   :max-generations      300
   :umad-rate            0.1
   :min-genome-size      50
   :max-genome-size      250
   :gene-distribution    {:open-close    0.1
                          :var           0.2
                          :local         0.15
                          :lit           0.15
                          :lit-generator 0.1
                          :abstraction   0.15
                          :apply         0.15}
   :penalty              1e5
   :simplification-steps 2000
   :downsample-rate      1.0})

(defn make-breed
  [opts]
  (let [select (tb/make-lexicase-selection opts)
        mutate (tb/make-size-neutral-umad opts)]
    (fn [generation]
      (->> (select generation) :genome mutate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def problem-info
  {"checksum"
   {:input->type    {'input1 string?}
    :return-type    string?
    :other-types    [int? boolean? char?]
    :literals       ["Check sum is " \space 64]
    :lit-generators [(pu/int-generator 128)
                     #(rand-nth (concat [\newline \tab] (map char (range 32 127))))]
    :loss-fns       [lev/distance
                     #(if (not (empty? %1))
                        (Math/abs (- (int (last %2)) (int (last %1)))) ;distance from correct last character
                        (:penalty config))]}

   "collatz-numbers"
   {:input->type    {'input1 int?}
    :return-type    int?
    :other-types    [int? float? boolean?]
    :literals       [0 1]
    :lit-generators [(pu/int-generator 100)]
    :loss-fns       [pu/absolute-distance]}

   "compare-string-lengths"
   {:input->type    {'input1 string?
                     'input2 string?
                     'input3 string?}
    :return-type    boolean?
    :other-types    [int?]
    :literals       []
    :lit-generators [pu/rand-bool]
    :loss-fns       [#(if (= %1 %2) 0 1)]}

   "count-odds"
   {:input->type    {'input1 [:vector int?]}
    :return-type    int?
    :other-types    [boolean?]
    :literals       [0 1 2]
    :lit-generators [(pu/int-generator 1000)]
    :loss-fns       [pu/absolute-distance]}

   "digits"
   {:input->type    {'input1 int?}
    :return-type    string?
    :other-types    [boolean? char?]
    :literals       [\newline]
    :lit-generators [(pu/int-generator 10)]
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
                            (:penalty config))))]}

   "last-index-of-zero"
   {:input->type    {'input1 [:vector int?]}
    :return-type    int?
    :other-types    [boolean?]
    :literals       []
    :lit-generators [(pu/int-generator 50)]
    :loss-fns       [pu/absolute-distance]}

   "median"
   {:input->type    {'input1 int?
                     'input2 int?
                     'input3 int?}
    :return-type    int?
    :other-types    [boolean?]
    :literals       []
    :lit-generators [(pu/int-generator 100)]
    :loss-fns       [pu/absolute-distance]}

   "mirror-image"
   {:input->type    {'input1 [:vector int?]
                     'input2 [:vector int?]}
    :return-type    boolean?
    :other-types    [int?]
    :literals       []
    :lit-generators [pu/rand-bool]
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
    :lit-generators [(pu/int-generator 100)
                     #(- (rand 201.0) 100.0)]
    :loss-fns       [#(try
                        (Math/abs (- (Double/parseDouble %1) %2))
                        (catch Exception e (:penalty config)))
                     #(lev/distance %1 (pr-str (pu/round 4 %2)))]}

   ; "pig-latin"

   "replace-space-with-newline"
   {:input->type    {'input1 string?}
    :return-type    int?
    ;; The `nil?` functions are side effects, like printing.
    :other-types    [int? boolean? char? nil?]
    :literals       [\space \newline]
    :lit-generators [#(rand-nth (concat [\newline \tab] (map char (range 32 127))))]
    :loss-fns       [pu/absolute-distance]
    ;; Config for how to unpack the cases from data files.
    :out-key        :output2
    :stdout-key     :output1}

   ; "scrabble-score"

   "small-or-large"
   {:input->type    {'input1 int?}
    :return-type    string?
    :other-types    [boolean?]
    :literals       ["small" "large"]
    :lit-generators [(pu/int-generator 10000)]
    :loss-fns       [lev/distance]}

   "smallest"
   {:input->type    {'input1 int?
                     'input2 int?
                     'input3 int?
                     'input4 int?}
    :return-type    int?
    :other-types    [boolean?]
    :literals       []
    :lit-generators [(pu/int-generator 100)]
    :loss-fns       [pu/absolute-distance]}

   ; "string-differences"

   "string-lengths-backwards"
   {:input->type    {'input1 [:vector string?]}
    :return-type    string?
    :other-types    [string? int? boolean? [:vector string?]]
    :literals       []
    :lit-generators [(pu/int-generator 100)]
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
                          (pu/absolute-distance num (parse %2))
                          (:penalty config)))]}

   "vector-average"
   {:input->type    {'input1 [:vector float?]}
    :return-type    float?
    :other-types    [int?]
    :literals       []
    :lit-generators []
    :loss-fns       [#(pu/round 4 (pu/absolute-distance %1 %2))]}

   "vectors-summed"
   {:input->type    {'input1 [:vector int?]
                     'input2 [:vector int?]}
    :return-type    [:vector int?]
    :other-types    [int?]
    :literals       []
    :lit-generators []
    :loss-fns       [#(reduce + (map pu/absolute-distance %1 %2))
                     #(* 1000 (pu/absolute-distance (count %1) (count %2)))]}

   ; "wallis-pi"
   ; "word-stats"
   ; "x-word-lines"

   })

(defn run
  [{:keys [dataset-dir problem]}]
  {:pre [(contains? problem-info problem)]}
  (let [info (problem-info problem)
        {:keys [train test]} (pu/psb1-read-examples dataset-dir problem (:n-train config) (:n-test config))
        reshape-case #(pu/reshape-case % info)
        train-cases (map reshape-case train)
        genetic-source (pu/genetic-source (assoc info :gene-distribution (:gene-distribution config)))
        breed (make-breed {:rate 0.1 :genetic-source genetic-source})
        {:keys [best result]} (gp/run (merge config
                                             info
                                             {:vars           (set (keys (lib/lib-for-types (pu/problem-types info))))
                                              :cases          train-cases
                                              :breed          breed
                                              :genetic-source genetic-source}))
        {:keys [total-error]} (gp/evaluate-code {:code        (:code best)
                                                 :arg-symbols (vec (sort (keys (:input->type info))))
                                                 :cases       (map reshape-case test)
                                                 :loss-fns    (:loss-fns info)
                                                 :penalty     (:penalty config)})]
    (log/info "BEST INDIVIDUAL" best)
    (log/info "BEST CODE" (reverse (into '() (:code best))))
    (if (= :solution-found result)
      (do
        (log/info "SOLUTION FOUND")
        (if (zero? total-error)
          (log/info "SOLUTION GENERALIZED")
          (log/info "SOLUTION FAILED TO GENERALIZE")))
      (log/info "SOLUTION NOT FOUND"))
    (:func best)))


(defn -main
  [dataset-dir problem]
  (run {:dataset-dir dataset-dir
        :problem     problem}))

(comment
  (require '[erp12.cbgp-lite.gp.pluhsy :as pl])
  (require '[erp12.cbgp-lite.lang.compile :as c])

  (try
    (run {:dataset-dir "data/program-synthesis-benchmark-datasets/datasets"
          :problem     "vectors-summed"})
    (catch Exception e
      (def caught e)))

  (type caught)
  (throw caught)

  (type (ex-cause caught))
  (throw (ex-cause caught))

  (def genome
    (:genome (ex-data (ex-cause caught))))
  genome

  (def genome
    [[:var 'do2]
     [:var 'str]
     :apply])

  (def push
    (pl/plushy->push genome))
  push

  (def code
    (c/push->clj {:push      push
                  :inputs    ['input1 'input2]
                  :ret-type  string?
                  :type-env  (->> lib/library
                                  (merge {'input1 float?
                                          'input2 int?})
                                  (mapv (fn [[symb typ]] [:= symb typ])))
                  :dealiases lib/dealiases}))

  code

  )
