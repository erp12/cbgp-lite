(ns erp12.cbgp-lite.benchmark.suite.composite
  (:require [clj-fuzzy.levenshtein :as lev] 
            [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.lib :as lib]))

(defn rand-int-range
  "Returns random int between low and high, both inclusive."
  [low high]
  (+ low (rand-int (inc (- high low)))))

;;;;;;;;;;;;;;;
;; Case generator functions

(defn add-them-case-generator
  []
  (let [x (rand-int-range -1000 1000)
        y (rand-int-range -1000 1000)]
    {:inputs [x y]
     :output (+ x y)}))

(defn sum-2-vals-solution-fn
  [input1 input2 input3]
  (+ (get input1 input2)
     (get input1 input3)))

(defn sum-2-vals-case-generator-generic
  "Produce a map of inputs and outputs.
   Works with any key generator functyion key-gen"
  [key-gen]
  (let [key1 (key-gen)
        key2 (key-gen)
        num-kvs (rand-int 49) ; along with two guaranteed keys, this makes at most 50 kv pairs
        the-keys (repeatedly num-kvs key-gen)
        the-vals (repeatedly num-kvs #(rand-int-range -1000 1000))
        input-map (assoc (zipmap the-keys the-vals)
                         key1 (rand-int-range -1000 1000)
                         key2 (rand-int-range -1000 1000))
        inputs [input-map key1 key2]]
    {:inputs inputs
     :output (apply sum-2-vals-solution-fn inputs)}))

(defn sum-2-vals-case-generator
  "Produce a map of inputs and outputs"
  []
  (sum-2-vals-case-generator-generic (bu/string-generator 10)))

(defn sum-2-vals-polymorphic-case-generator
  []
  (let [all-generators [(bu/string-generator 10)
                        (bu/int-generator 1000)
                        #(bu/rand-char)
                        rand
                        #(vec (repeatedly (inc (rand-int 16)) bu/rand-bool)) ;; vector of booleans
                        #(list (bu/rand-char) (rand-int-range -10 10)) ;; tuple containing a char and an integer
                        ]]
    (sum-2-vals-case-generator-generic (rand-nth all-generators))))

(defn sum-2D-case-generator
  []
  (let [rows (inc (rand-int 10))
        cols (inc (rand-int 10))
        input-matrix (vec (for [_ (range rows)]
                            (vec (repeatedly cols (bu/int-generator 1000)))))
        output (apply + (map #(apply + %) input-matrix))]
    {:inputs input-matrix
     :output output}))

(defn problems
  [{:keys [penalty]}]
  (let [penalize-nil (fn [loss-fn]
                       (fn wrapped-loss [program-output correct-output]
                         (if (or (nil? program-output)
                                 ; is a sequence and contains nil
                                 (and (coll? program-output)
                                      (some nil? program-output)))
                           penalty
                           (loss-fn program-output correct-output))))]
    (update-vals
     {"add-them"
      {:description "simple problem to make sure this file is working"
       :input->type {'input1 {:type 'int?}
                     'input2 {:type 'int?}}
       :ret-type    {:type 'int?}
       :other-types [{:type 'boolean?}]
       :extra-genes [{:gene :lit, :val 0, :type {:type 'int?}}]
       :case-generator add-them-case-generator
       :loss-fns    [bu/absolute-distance]}

      "sum-2-vals"
      {:description "Given a map from strings to ints and two strings that are
                     keys of the map, look up the values associated with those keys
                     in the map and return their sum."
       :input->type {'input1 {:type :map-of, :key {:type 'string?}, :value {:type 'int?}}
                     'input2 {:type 'string?}
                     'input3 {:type 'string?}}
       :ret-type    {:type 'int?}
       :other-types [{:type 'boolean?}]
       :extra-genes [{:gene :lit, :val 0, :type {:type 'int?}}
                     {:gene :lit-generator,
                      :fn   (bu/string-generator 21),
                      :type {:type 'string?}}]
       :case-generator sum-2-vals-case-generator
       :loss-fns    [bu/absolute-distance]}

      "sum-2-vals-polymorphic"
      {:description "Given a map from 'a to ints and two 'a that are
                     keys of the map, look up the values associated with those keys
                     in the map and return their sum."
       :input->type {'input1 {:type :map-of, :key (lib/s-var 'a), :value {:type 'int?}}
                     'input2 (lib/s-var 'a)
                     'input3 (lib/s-var 'a)}
       :ret-type    {:type 'int?}
       :other-types [{:type 'boolean?} {:type 'string?} {:type 'char?} {:type 'double?}]
       :extra-genes [{:gene :lit, :val 0, :type {:type 'int?}}]
       :case-generator sum-2-vals-polymorphic-case-generator
       :loss-fns    [bu/absolute-distance]}

      "sum-2D"
      {:description "Given 2D vector of ints (i.e. vector of vector of ints), return sum of all ints."
       :input->type {'input1 {:type :vector :child {:type :vector :child {:type 'int?}}}}
       :ret-type    {:type 'int?}
       :other-types [{:type :vector :child {:type 'int?}} {:type 'boolean?}]
       :extra-genes [{:gene :lit, :val 0, :type {:type 'int?}}]
       :case-generator sum-2D-case-generator
       :loss-fns    [bu/absolute-distance]}}

     ;; This adds nil penalties to all loss functions
     (fn [problem-map]
       (update problem-map :loss-fns #(map penalize-nil %))))))

(defn read-cases
  [{:keys [problem n-train n-test]}] [problem :case-generator]
  (let [case-generator (get-in (problems nil)
                               [(name problem) :case-generator])]
    {:train (repeatedly n-train case-generator)
     :test  (repeatedly n-test case-generator)}))

(comment

  (sum-2D-case-generator)


  )
