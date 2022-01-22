(ns erp12.cbgp-lite.benchmark.psb
  (:gen-class)
  (:require [erp12.ga-clj.toolbox :as tb]
            [erp12.cbgp-lite.gp :as gp]
            [erp12.cbgp-lite.benchmark.psb-utils :as pu]
            [erp12.cbgp-lite.lang.lib :as lib]
            [taoensso.timbre :as log]
            [clj-fuzzy.levenshtein :as lev]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(def config
  {:n-train              200
   :n-test               300
   :population-size      1000
   ;; 10x generations because down-sampling to 10% of cases per generation.
   :max-generations      3000
   :umad-rate            0.1
   :min-genome-size      50
   :max-genome-size      250
   :gene-distribution    {:open-close    0.1
                          :input         0.15
                          :var           0.2
                          :lit           0.15
                          :lit-generator 0.1
                          :abstraction   0.15
                          :apply         0.15}
   :penalty              1e5
   :simplification-steps 2000
   :downsample-rate      0.1})

(defn make-breed
  [opts]
  (let [select (tb/make-lexicase-selection opts)
        mutate (tb/make-size-neutral-umad opts)]
    (fn [generation]
      (->> (select generation) :genome mutate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def problem-info
  {"compare-string-lengths"
   {:input->type    {'input1 string?
                     'input2 string?
                     'input3 string?}
    :return-type    boolean?
    :other-types    [int?]
    :literals       []
    :lit-generators [pu/rand-bool]
    :loss-fn        #(if (= %1 %2) 0 1)}

   "count-odds"
   {:input->type    {'input1 [:vector int?]}
    :return-type    int?
    :other-types    [boolean?]
    :literals       [0 1 2]
    :lit-generators [#(- (rand-int 2001) 1000)]
    :loss-fn        pu/absolute-distance}

   "double-letters"
   {:input->type    {'input1 string?}
    :return-type    string?
    :other-types    [int? boolean? char?]
    :literals       [\!]
    :lit-generators []
    :loss-fn        lev/distance}

   "last-index-of-zero"
   {:input->type    {'input1 [:vector int?]}
    :return-type    int?
    :other-types    [boolean?]
    :literals       []
    :lit-generators [#(- (rand-int 101) 50)]
    :loss-fn        pu/absolute-distance}

   "median"
   {:input->type    {'input1 int?
                     'input2 int?
                     'input3 int?}
    :return-type    int?
    :other-types    [boolean?]
    :literals       []
    :lit-generators [#(- (rand-int 201) 100)]
    :loss-fn        pu/absolute-distance}

   "mirror-image"
   {:input->type    {'input1 [:vector int?]
                     'input2 [:vector int?]}
    :return-type    boolean?
    :other-types    [int?]
    :literals       []
    :lit-generators [pu/rand-bool]
    :loss-fn        #(if (= %1 %2) 0 1)}

   "negative-to-zero"
   {:input->type    {'input1 [:vector int?]}
    :return-type    [:vector int?]
    :other-types    [int? boolean?]
    :literals       []
    :lit-generators []
    :loss-fn        lev/distance}

   "number-io"
   {:input->type    {'input1 float?
                     'input2 int?}
    :return-type    float?
    :other-types    []
    :literals       []
    :lit-generators [#(- (rand-int 201) 100)
                     #(- (rand 201.0) 100.0)]
    :loss-fn        pu/absolute-distance}

   "replace-space-with-newline"
   {:input->type    {'input1 string?}
    :return-type    string?
    :other-types    [int? boolean? char?]
    :literals       [\space \newline]
    :lit-generators [#(rand-nth (concat [\newline \tab] (map char (range 32 127))))]
    :loss-fn        lev/distance}

   "small-or-large"
   {:input->type    {'input1 int?}
    :return-type    string?
    :other-types    [boolean?]
    :literals       ["small" "large"]
    :lit-generators [#(- (rand-int 20001) 10000)]
    :loss-fn        lev/distance}

   "smallest"
   {:input->type    {'input1 int?
                     'input2 int?
                     'input3 int?
                     'input4 int?}
    :return-type    int?
    :other-types    [boolean?]
    :literals       []
    :lit-generators [#(- (rand-int 201) 100)]
    :loss-fn        pu/absolute-distance}

   "string-lengths-backwards"
   {:input->type    {'input1 [:vector string?]}
    :return-type    string?
    :other-types    [string? int? boolean? [:vector string?]]
    :literals       []
    :lit-generators [#(- (rand-int 201) 100)]
    :loss-fn        lev/distance}

   "vector-average"
   {:input->type    {'input1 [:vector float?]}
    :return-type    float?
    :other-types    [int?]
    :literals       []
    :lit-generators []
    :loss-fn        pu/absolute-distance}

   "vector-summed"
   {:input->type    {'input1 [:vector int?]
                     'input2 [:vector int?]}
    :return-type    [:vector int?]
    :other-types    [int?]
    :literals       []
    :lit-generators []
    :loss-fn        pu/absolute-distance}

   })

(defn run
  [{:keys [dataset-dir problem]}]
  {:pre [(contains? problem-info problem)]}
  (let [info (problem-info problem)
        {:keys [train test]} (pu/psb1-read-examples dataset-dir problem (:n-train config) (:n-test config))
        train-cases (map pu/reshape-case train)
        genetic-source (pu/genetic-source (assoc info :gene-distribution (:gene-distribution config)))
        breed (make-breed {:rate           0.1
                           :genetic-source genetic-source})
        {:keys [best result]} (gp/run (merge config
                                             info
                                             {:vars           (set (keys (lib/lib-for-types (pu/problem-types info))))
                                              :cases          train-cases
                                              :breed          breed
                                              :genetic-source genetic-source}))
        test-cases (map pu/reshape-case test)
        y-pred (map #(apply (:func best) (:inputs %)) test-cases)
        y-true (map :output test-cases)
        test-errors (map (:loss-fn info) y-pred y-true)
        test-error (apply + test-errors)]
    (log/info "BEST INDIVIDUAL" best)
    (log/info "BEST CODE" (reverse (into '() (:code best))))
    (if (= :solution-found result)
      (do
        (log/info "SOLUTION FOUND")
        (if (zero? test-error)
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
          :problem     "number-io"})
    (catch Exception e
      (def caught e)))

  (type caught)
  (throw caught)

  (type (ex-cause caught))
  (throw (ex-cause caught))

  (def genome
    (:genome (ex-data caught)))
  genome

  (def push
    (pl/plushy->push genome))
  push

  (def code
    (c/push->clj {:push      push
                  :inputs    ['input1]
                  :ret-type  int?
                  :type-env  (->> lib/library
                                  (merge {'input1 [:vector int?]})
                                  (mapv (fn [[symb typ]] [:= symb typ])))
                  :dealiases lib/dealiases}))

  code

  )
