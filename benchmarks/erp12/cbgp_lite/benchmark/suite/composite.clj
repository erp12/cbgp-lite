(ns erp12.cbgp-lite.benchmark.suite.composite
  (:require [clj-fuzzy.levenshtein :as lev]
            [clojure.set :as set]
            [clojure.set :as st]
            [clojure.string :as str]
            [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.lib :as lib]))

(defn rand-int-range
  "Returns random int between low and high, both inclusive."
  [low high]
  (+ low (rand-int (inc (- high low)))))

(defn rand-float-range
  "Returns random float between low and high, both inclusive."
  [low high]
  (+ low (rand (- high low))))

(defn rand-vector
  [min-size max-size element-gen]
  (vec (repeatedly (rand-float-range min-size max-size)
                   element-gen)))

(def int-predicates
  [zero?
   pos?
   neg?
   even?
   odd?])

(def double-predicates
  [zero?
   pos?
   neg?
   (fn mag-10? [x] (and (< -10 x) (< x 10)))])

(def bool-predicates
  [true? false?])

(def string-predicates
  [empty?
   distinct?
   (fn has-space? [s] (str/includes? s " "))
   (fn >5-chars? [s] (> (count s) 5))
   (fn even-chars? [s] (zero? (mod (count s) 2)))])

(def char-predicates
  [lib/whitespace?
   lib/digit?
   lib/letter?
   (fn upper-case? [c] (= (str c) (str/upper-case c)))
   (fn lower-case? [c] (= (str c) (str/lower-case c)))])

(def set-of-ints-predicates
  [empty?
   (fn has-zero? [s] (contains? s 0))
   (fn has-even? [s] (some even? s))
   (fn has-neg? [s] (some neg? s))
   (fn >10-size? [s] (> (count s) 10))
   (fn has-012? [s] (set/subset? #{0 1 2} s))])

;;;;;;;;;;;;;;;
;; Case generator functions

(defn sum-2-vals-case-generator
  "Produce a map of inputs and outputs.
   Works with any key generator functyion key-gen"
  [key-gen]
  (let [key1 (key-gen)
        key2 (key-gen)
        ; along with two guaranteed keys, this makes at most 50 kv pairs
        num-kvs (rand-int 49)
        the-keys (repeatedly num-kvs key-gen)
        the-vals (repeatedly num-kvs #(rand-int-range -1000 1000))
        input-map (assoc (zipmap the-keys the-vals)
                    key1 (rand-int-range -1000 1000)
                    key2 (rand-int-range -1000 1000))]
    {:inputs [input-map key1 key2]
     :output (+ (get input-map key1) (get input-map key2))}))

(def value-generators-and-predicates
  [{:val-gen (bu/int-generator 100) :preds int-predicates}
   {:val-gen #(- (rand 200.0) 100.0) :preds double-predicates}
   {:val-gen bu/rand-bool :preds bool-predicates}
   {:val-gen bu/rand-char :preds char-predicates}
   {:val-gen (bu/string-generator 12) :preds string-predicates}
   {:val-gen #(set (repeatedly (rand-int 12) (bu/int-generator 100)))
    :preds   set-of-ints-predicates}])

(defn make-int-to-int-fn
  [bound]
  (let [rand-map (zipmap (range bound) (shuffle (range bound)))
        options [(fn [x] (+ (- (abs (- (rand-int bound) x)))
                            (rand-int-range -50 50)))
                 (fn [x] (let [a (rand-int bound)]
                           (+ (* -1 (- a x) (- a x))
                              (rand-int-range -50 50))))
                 (fn [x] (get rand-map x))]]
    (rand-nth options)))

(defn max-applied-fn-case-generator
  "Given an integer X < 50 and a (int => int) function, return
   the integer in [0, X) that results in the maximum value for the function."
  []
  (let [bound (rand-int-range 1 49)
        the-fn (make-int-to-int-fn bound)
        output (first (apply max-key second (map #(list % (the-fn %))
                                                 (range bound))))]
    {:inputs [bound the-fn]
     :output output}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Problems ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn problems
  [{:keys [penalty]}]
  (let [penalize-nil (fn [loss-fn]
                       (fn wrapped-loss [program-output correct-output]
                         (if (bu/has-nil? program-output)
                           penalty
                           (loss-fn program-output correct-output))))]
    (update-vals
      {"add-them"
       {:description    "simple problem to make sure this file is working"
        :input->type    {'input1 {:type 'int?}
                         'input2 {:type 'int?}}
        :ret-type       {:type 'int?}
        :other-types    [{:type 'boolean?}]
        :extra-genes    [{:gene :lit, :val 0, :type {:type 'int?}}]
        :case-generator (fn add-them-gen []
                          (let [x (rand-int-range -1000 1000)
                                y (rand-int-range -1000 1000)]
                            {:inputs [x y]
                             :output (+ x y)}))
        :loss-fns       [bu/absolute-distance]}

       "sum-2-vals"
       {:description    "Given a map from strings to ints and two strings that are
                     keys of the map, look up the values associated with those keys
                     in the map and return their sum."
        :input->type    {'input1 {:type :map-of, :key {:type 'string?}, :value {:type 'int?}}
                         'input2 {:type 'string?}
                         'input3 {:type 'string?}}
        :ret-type       {:type 'int?}
        :other-types    [{:type 'boolean?}]
        :extra-genes    [{:gene :lit, :val 0, :type {:type 'int?}}
                         {:gene :lit-generator,
                          :fn   (bu/string-generator 21),
                          :type {:type 'string?}}]
        :case-generator (fn sum-2-vals-gen []
                          (sum-2-vals-case-generator (bu/string-generator 10)))
        :loss-fns       [bu/absolute-distance]}

       "sum-2-vals-polymorphic"
       {:description    "Given a map from 'a to ints and two 'a that are
                     keys of the map, look up the values associated with those keys
                     in the map and return their sum."
        :input->type    {'input1 {:type :map-of, :key {:type 'T}, :value {:type 'int?}}
                         'input2 {:type 'T}
                         'input3 {:type 'T}}
        :ret-type       {:type 'int?}
        :other-types    [{:type 'boolean?} {:type 'string?} {:type 'char?} {:type 'double?}]
        :extra-genes    [{:gene :lit, :val 0, :type {:type 'int?}}]
        :case-generator (let [key-generators [(bu/string-generator 10)
                                              (bu/int-generator 1000)
                                              #(bu/rand-char)
                                              rand
                                              ;; vector of booleans
                                              #(vec (repeatedly (inc (rand-int 16)) bu/rand-bool))
                                              ;; tuple containing a char and an integer
                                              #(vector (bu/rand-char) (rand-int-range -10 10))]]
                          (fn sum-2-vals-polymorphic-gen []
                            (sum-2-vals-case-generator (rand-nth key-generators))))
        :loss-fns       [bu/absolute-distance]}

       "sum-2D"
       {:description    "Given 2D vector of ints (i.e. vector of vector of ints), return sum of all ints."
        :input->type    {'input1 {:type :vector :child {:type :vector :child {:type 'int?}}}}
        :ret-type       {:type 'int?}
        :other-types    [{:type :vector :child {:type 'int?}} {:type 'boolean?}]
        :extra-genes    [{:gene :lit, :val 0, :type {:type 'int?}}]
        :case-generator (fn sum-2D-gen []
                          (let [rows (inc (rand-int 10))
                                cols (inc (rand-int 10))
                                input-matrix (vec (for [_ (range rows)]
                                                    (vec (repeatedly cols (bu/int-generator 1000)))))
                                output (reduce + (map #(reduce + %) input-matrix))]
                            {:inputs [input-matrix]
                             :output output}))
        :loss-fns       [bu/absolute-distance]}

       ;; TODO: Rerun after figuring out error in some runs
       "centimeters-to-meters"
       {:description    "Given a length in centimeters, return a tuple of (meters, centimeters)
                     that corresponds to the same length."
        :input->type    {'input1 {:type 'int?}}
        :ret-type       {:type :tuple, :children [{:type 'int?} {:type 'int?}]}
        :other-types    [{:type 'boolean?}]
        :extra-genes    [{:gene :lit, :val 0, :type {:type 'int?}}
                         {:gene :lit, :val 100, :type {:type 'int?}}]
        :case-generator (fn centimeters-to-meters-gen []
                          (let [in-cm (rand-int 10000)
                                out-m (quot in-cm 100)
                                out-cm (mod in-cm 100)]
                            {:inputs [in-cm]
                             :output (vector out-m out-cm)}))
        :loss-fns       [#(bu/absolute-distance (first %1) (first %2))
                         #(bu/absolute-distance (second %1) (second %2))]}

       ;; TODO: Rerun after figuring out error in some runs
       "set-symmetric-difference"
       {:description    "Given two sets, find the symmetric difference
                     https://en.wikipedia.org/wiki/Symmetric_difference "
        :input->type    {'input1 {:type :set :child {:type 'int?}}
                         'input2 {:type :set :child {:type 'int?}}}
        :ret-type       {:type :set :child {:type 'int?}}
        :other-types    [{:type 'boolean?} {:type 'int?}]
        :extra-genes    [{:gene :lit, :val #{}, :type {:type :set :child {:type 'int?}}}]
        :case-generator (fn set-symmetric-difference-gen
                          []
                          (let [set-generator (fn [] (set (repeatedly (rand-int 50) #(rand-int 50))))
                                set1 (set-generator)
                                set2 (set-generator)
                                output (st/union (st/difference set1 set2)
                                                 (st/difference set2 set1))]
                            {:inputs [set1 set2]
                             :output output}))
        :loss-fns       [bu/jaccard-similarity-loss]}

       "max-applied-fn"
       {:description    "Given an integer X < 50 and a (int => int) function, return
                     the integer in [0, X) that results in the maximum value for
                     the function."
        :input->type    {'input1 {:type 'int?}
                         'input2 (lib/unary-transform {:type 'int?})}
        :ret-type       {:type 'int?}
        :other-types    [{:type 'boolean?}]
        :extra-genes    [{:gene :lit, :val 0, :type {:type 'int?}}
                         {:gene :lit, :val true, :type {:type 'boolean?}}
                         {:gene :lit, :val false, :type {:type 'boolean?}}]
        :case-generator max-applied-fn-case-generator
        :loss-fns       [bu/absolute-distance]}

       "count-true"
       {:description    "Given a vector of T and a predicate T => bool, return the
                     count of the number of elements in T that make the predicate true."
        :input->type    {'input1 {:type :vector :child {:type 'T}}
                         'input2 (lib/unary-pred {:type 'T})}
        :ret-type       {:type 'int?}
        :other-types    [{:type 'boolean?}]
        :extra-genes    [{:gene :lit, :val 0, :type {:type 'int?}}
                         {:gene :lit, :val true, :type {:type 'boolean?}}
                         {:gene :lit, :val false, :type {:type 'boolean?}}]
        :case-generator (fn []
                          (let [{:keys [val-gen preds]} (rand-nth value-generators-and-predicates)
                                vector (rand-vector 0 50 val-gen)
                                pred (rand-nth preds)]
                            {:inputs [vector pred]
                             :output (count (filter pred vector))}))
        :loss-fns       [bu/absolute-distance]}

       "first-index-of-true"
       {:description    "Given a vector of T and a predicate T => bool, return the
                     first index in the vector where the predicate is true."
        :input->type    {'input1 {:type :vector :child {:type 'T}}
                         'input2 (lib/unary-pred {:type 'T})}
        :ret-type       {:type 'int?}
        :other-types    [{:type 'boolean?}]
        :extra-genes    [{:gene :lit, :val -1, :type {:type 'int?}}
                         {:gene :lit, :val 0, :type {:type 'int?}}
                         {:gene :lit, :val true, :type {:type 'boolean?}}
                         {:gene :lit, :val false, :type {:type 'boolean?}}]
        :case-generator (fn []
                          (let [{:keys [val-gen preds]} (rand-nth value-generators-and-predicates)
                                the-vector (rand-vector 0 50 val-gen)
                                pred (rand-nth preds)
                                output (->> the-vector
                                            (map-indexed vector)
                                            (filter #(pred (second %)))
                                            ffirst)]
                            {:inputs [vector pred]
                             :output (or output -1)}))
        :loss-fns       [bu/absolute-distance]}

       "set-cartesian-product"
       {:description    "Given two sets, find their cartesian product, which will be a set of tuples."
        :input->type    {'input1 {:type :set :child {:type 'int?}}
                         'input2 {:type :set :child {:type 'int?}}}
        :ret-type       {:type :set :child {:type :tuple, :children [{:type 'int?} {:type 'int?}]}}
        :other-types    [{:type 'boolean?} {:type 'int?}]
        :extra-genes    [{:gene :lit, :val 0, :type {:type 'int?}}]
        :case-generator (let [set-generator (fn [] (set (rand-vector 0 21 #(rand-int 100))))]
                          (fn cartesian-product-gen
                            []
                            (let [set1 (set-generator)
                                  set2 (set-generator)
                                  output (set (for [x set1
                                                    y set2]
                                                (vector x y)))]
                              {:inputs [set1 set2]
                               :output output})))
        :loss-fns       [bu/jaccard-similarity-loss]}

       ;; TODO: Figure out loss-fns
       "filter-bounds"
       {:description    "Given a vector of elements that are all of the same comparable
                     type, T , and two instance of type T representing a lower and
                     upper bound, filter the list to the elements that fall
                     between two bounds (inclusively)."
        :input->type    {'input1 {:type :vector :child {:type 'T}}
                         'input2 {:type 'T}
                         'input3 {:type 'T}}
        :ret-type       {:type :vector :child {:type 'T}}
        :other-types    [{:type 'boolean?} {:type 'int?}]
        :extra-genes    [{:gene :lit, :val [], :type {:type :vector :child {:type 'T}}}]
        :case-generator (let [generators [(bu/string-generator 10)
                                          #(bu/rand-char)
                                          (bu/int-generator 1000)
                                          (bu/int-generator 20)
                                          rand]]
                          (fn filter-bounds-gen []
                            (let [val-gen (rand-nth generators)
                                  the-vector (rand-vector 20 50 val-gen)
                                  x (val-gen)
                                  y (val-gen)
                                  lower (lib/min' x y)
                                  upper (lib/max' x y)]
                              {:inputs [the-vector lower upper]
                               :output (filterv #(and (lib/<' lower %) (lib/<' % upper))
                                                the-vector)})))
        :loss-fns       [(fn [x y] (bu/absolute-distance (count x) (count y)))
                         lev/distance]}

       "area-of-rectangle"
       {:description    "Given two tuples of floats representing the upper-right and
                     lower-left coordinates of a rectangle in the cartesian plane,
                     find the area of the rectangle."
        :input->type    {'input1 {:type :tuple, :children [{:type 'double?} {:type 'double?}]}
                         'input2 {:type :tuple, :children [{:type 'double?} {:type 'double?}]}}
        :ret-type       {:type 'double?}
        :other-types    []
        :extra-genes    []
        :case-generator (fn area-of-rectangle-gen
                          []
                          (let [xs [(rand-float-range -100 100) (rand-float-range -100 100)]
                                ys [(rand-float-range -100 100) (rand-float-range -100 100)]
                                x1 (reduce max xs)
                                x2 (reduce min xs)
                                y1 (reduce max ys)
                                y2 (reduce min ys)
                                output (* (- x1 x2)
                                          (- y1 y2))]
                            {:inputs [[x1 y1] [x2 y2]]
                             :output output}))
        :loss-fns       [#(bu/round 4 (bu/absolute-distance %1 %2))]}

       }

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

  (compare [false false] [true false false])

  )
