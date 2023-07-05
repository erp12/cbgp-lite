(ns erp12.cbgp-lite.benchmark.utils
  (:require [clojure.set :as st]
            [clojure.walk :as w]
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
     (let [cfh (->> x->freq
                    (sort-by key)
                    (reductions (fn [[_ acc] [x freq]]
                                  [x (+ acc freq)])
                                [nil 0]))
           total-freq (second (last cfh))
           quart (int (/ (inc total-freq) 4))]
       {:mean (float (/ (reduce + (map (fn [[x freq]] (* x freq)) x->freq))
                        total-freq))
        :min  (reduce min (keys x->freq))
        :25%  (some (fn [[x c-freq]] (when (> c-freq quart) x)) cfh)
        :50%  (some (fn [[x c-freq]] (when (> c-freq (* quart 2)) x)) cfh)
        :75%  (some (fn [[x c-freq]] (when (> c-freq (* quart 3)) x)) cfh)
        :max  (reduce max (keys x->freq))}))
    ;; Reduce
    ([acc el]
     (update acc (by el) (fn [i] (inc (or i 0)))))))

(def total-error-stat
  (make-distribution-stat :total-error))

(def genome-size-stat
  (make-distribution-stat #(count (:genome %))))

(def code-size-stat
  (make-distribution-stat #(tb/tree-size (:code %))))

(def code-depth-stat
  (make-distribution-stat #(tb/tree-depth (:code %))))

(def code-depth-over-size-stat
  (make-distribution-stat #(/ (tb/tree-depth (:code %))
                              (tb/tree-size (:code %)))))

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

(defn round
  "Round a double to the given precision (number of significant digits)"
  [precision n] 
  (cond
    (nil? n) nil
    (NaN? n) (Math/round ##Inf)
    :else (let [factor (Math/pow 10 precision)]
            (/ (Math/round (* n factor)) factor))))

(defn absolute-distance
  [actual expected]
  (if (or (nil? actual) (nil? expected))
    nil
    (Math/abs (- actual expected))))

(defn vector-of-numbers-loss
  [actual expected]
  (if (or (nil? actual) (nil? expected))
    nil
    (+' (reduce +' (map (fn [cor res]
                          (absolute-distance cor res))
                        expected
                        actual))
        (*' 1000 (abs (- (count expected) (count actual)))))))

(defn jaccard-similarity-loss
  "this = (1 - Jaccard similarity coefficent), since we want lower to be better
   https://en.wikipedia.org/wiki/Jaccard_index "
  [actual expected]
  (cond
    (or (nil? actual) (nil? expected)) nil
    (= actual expected) 0 ; if equal (including both empty), 0 loss
    :else (- 1.0 (/ (count (st/intersection actual expected))
                    (count (st/union actual expected))))))