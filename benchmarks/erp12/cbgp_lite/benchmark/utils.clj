(ns erp12.cbgp-lite.benchmark.utils
  (:require [clojure.set :as st]
            [clojure.walk :as w]
            [erp12.cbgp-lite.lang.ast :as a]
            [erp12.ga-clj.toolbox :as tb]))

(defn read-problem
  [{:keys [suite-ns problem] :as config}]
  (require suite-ns)
  (let [suite-ns (find-ns suite-ns)
        suite-problems ((ns-resolve suite-ns 'problems) config)
        problem-info (get suite-problems (name problem))
        read-cases (ns-resolve suite-ns 'read-cases)]
    (merge config (read-cases config) problem-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Population Statistics

(defn aggregate-stats
  [stats coll]
  (->> coll
       (reduce (fn [stats' el]
                 ;; Accumulate 1 more element in the stats.
                 (map (fn [[k acc]]
                        (let [stat-fn (get stats k)
                              ;; @todo Cleanup try-catch after stats code becomes more stable.
                              acc' (try
                                     (stat-fn acc el)
                                     (catch Exception e
                                       (throw (ex-info "Failed to reduce stat for element."
                                                       {:stat k :element el}
                                                       e))))]
                          [k acc']))
                      stats'))
               ;; Initialize stats.
               (map (fn [[k f]] [k (f)]) stats))
       ;; Finalize stats.
       (map (fn [[k acc]]
              (let [stat-fn (get stats k)
                    ;; @todo Cleanup try-catch after stats code becomes more stable.
                    stat (try
                           (stat-fn acc)
                           (catch Exception e
                             (throw (ex-info "Failed to finalize stat."
                                             {:stat k :accumulated acc}
                                             e))))]
                [k stat])))
       (into {})))

(defn unique-behaviors-stat
  ;; Init
  ([] #{})
  ;; Finalize
  ([behaviors]
   (count behaviors))
  ;; Reduce
  ([behaviors {:keys [behavior]}]
   (conj behaviors behavior)))

(defn num-throwing-stat
  ;; Init
  ([] 0)
  ;; Finalize
  ([n] n)
  ;; Reduce
  ([n {:keys [exception]}]
   (+ n (if exception 1 0))))

(defn num-no-ast-stat
  ([] 0)
  ([n] n)
  ([n {:keys [code]}]
   (+ n (if code 0 1))))

(defn lowest-error-per-case
  ([] (repeat Long/MAX_VALUE))
  ([min-errors] min-errors)
  ([min-errors {:keys [errors]}]
   (mapv min min-errors errors)))

(defn make-distribution-stat
  [by]
  (fn
    ;; Initialize
    ([] {})
    ;; Finalize
    ([x->freq]
     (let [dont-include (get x->freq :dont-include-in-stats 0)
           x->freq (dissoc x->freq :dont-include-in-stats) ;; TMH: I changed this to ignore :dont-include-in-stats keys, so that we can have the stat functions return :dont-include-in-stats if we don't want that ind counted
           cfh (->> x->freq
                    (sort-by key)
                    (reductions (fn [[_ acc] [x freq]]
                                  [x (+ acc freq)])
                                [nil 0]))
           total-freq (second (last cfh))
           quart (int (/ (inc total-freq) 4))]

       (if (zero? total-freq)
         {:mean nil
          :dont-include-in-stats dont-include}
         {:mean (float (/ (reduce + (map (fn [[x freq]] (* (float x) freq)) x->freq))
                          total-freq))
          :min  (reduce min (keys x->freq))
          :25%  (some (fn [[x c-freq]] (when (> c-freq quart) x)) cfh)
          :50%  (some (fn [[x c-freq]] (when (> c-freq (* quart 2)) x)) cfh)
          :75%  (some (fn [[x c-freq]] (when (> c-freq (* quart 3)) x)) cfh)
          :max  (reduce max (keys x->freq))
          :dont-include-in-stats dont-include})))
    ;; Reduce
    ([acc el]
     (update acc (by el) (fn [i] (inc (or i 0)))))))

(def total-error-stat
  (make-distribution-stat :total-error))

(def applied-functions
  (make-distribution-stat :fn-applied))

(def genome-size-stat
  (make-distribution-stat #(count (:genome %))))

(def applied-stat
  (make-distribution-stat #(:fn-applied (:state %))))

(def not-applied-stat
  (make-distribution-stat #(:fn-not-applied (:state %))))

(def not-func-so-not-apply-stat
  (make-distribution-stat #(:fn-not-applied-because-no-functions (:state %))))

(def code-size-stat
  (make-distribution-stat #(tb/tree-size (:code %))))

(def code-depth-stat
  (make-distribution-stat #(tb/tree-depth (:code %))))

(def code-depth-over-size-stat
  (make-distribution-stat #(/ (tb/tree-depth (:code %))
                              (tb/tree-size (:code %)))))

(def dna-counter-stat
  (make-distribution-stat #(:dna (:state %))))

(def ast-stack-size-stat
  (make-distribution-stat #(count (:asts (:state %)))))

;; Returns the max tree size of any tree on the :asts stack, and if stack is
;; empty returns :dont-include-in-stats
(def ast-stack-max-tree-size
  (make-distribution-stat (fn [ind]
                            (if (empty? (:asts (:state ind)))
                              :dont-include-in-stats ;; this :dont-include-in-stats is necessary because some individuals produce empty stacks, so we can use :dont-include-in-stats to mean "don't include this one in the stats"
                              (apply max
                                     (map (fn [ast]
                                            (tb/tree-size
                                             (a/ast->form (:erp12.cbgp-lite.lang.compile/ast ast))))
                                          (:asts (:state ind))))))))

(def ast-stack-median-tree-size
  (make-distribution-stat (fn [ind]
                            ;; JF TODO: make this a let statement for better efficiency.
                            ;; use 
                            (if (empty? (:asts (:state ind)))
                              :dont-include-in-stats ;; this :dont-include-in-stats is necessary because some individuals produce empty stacks, so we can use :dont-include-in-stats to mean "don't include this one in the stats"
                              
                                (tb/median (map (fn [ast]
                                       (tb/tree-size
                                        (a/ast->form (:erp12.cbgp-lite.lang.compile/ast ast))))
                                     (:asts (:state ind))))
                               ))))


(def ast-stack-max-tree-size-for-right-type
  (make-distribution-stat (fn [ind]
                            (let [filtered-by-same-type (filter #(= (:ret-type ind) (:erp12.cbgp-lite.lang.compile/type %)) (:asts (:state ind)))]
                              (if (empty? filtered-by-same-type)
                                :dont-include-in-stats ;; this :dont-include-in-stats is necessary because some individuals produce empty stacks, so we can use :dont-include-in-stats to mean "don't include this one in the stats"
                                (apply max
                                       (map (fn [ast]
                                              (tb/tree-size
                                               (a/ast->form (:erp12.cbgp-lite.lang.compile/ast ast))))
                                            filtered-by-same-type)))))))

(def ast-stack-max-tree-depth
  (make-distribution-stat (fn [ind]
                            (if (empty? (:asts (:state ind)))
                              :dont-include-in-stats ;; this :dont-include-in-stats is necessary because some individuals produce empty stacks, so we can use :dont-include-in-stats to mean "don't include this one in the stats"
                              (apply max
                                     (map (fn [ast]
                                            (tb/tree-depth
                                             (a/ast->form (:erp12.cbgp-lite.lang.compile/ast ast))))
                                          (:asts (:state ind))))))))


(def ast-stack-max-tree-depth-for-right-type
  (make-distribution-stat (fn [ind]
                            (let [filtered-by-same-type (filter #(= (:ret-type ind) (:erp12.cbgp-lite.lang.compile/type %)) (:asts (:state ind)))]
                              (if (empty? filtered-by-same-type)
                                :dont-include-in-stats ;; this :dont-include-in-stats is necessary because some individuals produce empty stacks, so we can use :dont-include-in-stats to mean "don't include this one in the stats"
                                (apply max
                                       (map (fn [ast]
                                              (tb/tree-depth
                                               (a/ast->form (:erp12.cbgp-lite.lang.compile/ast ast))))
                                            filtered-by-same-type)))))))

(defn make-num-penalty-stat
  [penalty]
  (make-distribution-stat (fn [{:keys [errors]}] (count (filter #(= % penalty) errors)))))

(defn exception-messages-stat
  ;; Init
  ([] #{})
  ;; Finalize
  ([exs] exs)
  ;; Reduce
  ([exs {:keys [exception]}]
   (if exception
     (conj exs (str (.getName (class exception))
                    ": "
                    (ex-message exception)))
     exs)))

;; @todo (defn selections-per-parent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC Generators

(defn rand-bool
  []
  (> (rand) 0.5))

(defn int-generator
  [magnitude]
  #(- (rand-int (inc (* 2 magnitude))) magnitude))

(defn rand-char
  []
  (rand-nth (concat [\newline \tab] (map char (range 32 127)))))

(defn string-generator
  "Returns a generator of random strings of given max-length"
  [max-length]
  #(apply str
          (repeatedly (rand-int max-length)
                      rand-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loss Function Utils

(defn tap-nodes
  [f tree] 
    (w/walk (partial tap-nodes f) identity (f tree)))

(defn has-nil?
  [x]
  (with-local-vars [result false]
    (tap-nodes
     (fn [node]
       (when (nil? node)
         (var-set result true))
       node)
     x)
    @result))

(defn penalize-nil-and-exception
  "Wraps a loss function to check if the output contains nil or throws exception
   In those cases, returns penalty, otherwise applies loss-fn."
  [penalty loss-fn]
  (fn wrapped-loss [program-output correct-output]
    (if (has-nil? program-output)
      penalty
      (try
        (loss-fn program-output correct-output)
        (catch Exception e
          (println "Caught exception in loss function:" e)
          penalty)))))

(defn round
  "Round a double to the given precision (number of significant digits)"
  [precision n]
  (cond
    (nil? n) nil
    (NaN? n) (Math/round ##Inf)
    :else (let [factor (Math/pow 10 precision)]
            (/ (Math/round (* n factor)) factor))))

(defn abs'
  "Returns absolute value, coercing to bigint if necessary."
  [x]
  (if (neg? x)
    (-' x)
    x))

(defn absolute-distance
  [actual expected]
  (if (or (nil? actual) (nil? expected))
    nil
    (abs' (-' actual expected))))

(defn vector-of-numbers-loss
  [actual expected]
  (if (or (nil? actual) (nil? expected))
    nil
    (+' (reduce +' (map (fn [cor res]
                          (absolute-distance cor res))
                        expected
                        actual))
        (*' 1000 (abs (- (count expected) (count actual)))))))

;; @todo move to .
(defn jaccard-similarity-loss
  "this = (1 - Jaccard similarity coefficent), since we want lower to be better
   https://en.wikipedia.org/wiki/Jaccard_index "
  [actual expected]
  ;; (when (or (vector? actual) (vector? expected))
  ;;   (println "Actual:" actual)
  ;;   (println "Expected:" expected))
  (cond
    (or (nil? actual) (nil? expected)) nil
    (= actual expected) 0 ; if equal (including both empty), 0 loss
    :else (- 1.0 (/ (count (st/intersection actual expected))
                    (count (st/union actual expected))))))
