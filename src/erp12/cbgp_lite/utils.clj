(ns erp12.cbgp-lite.utils)

(defn first-non-nil
  "Returns the first non-nil values from the collection, or returns `nil` if
  the collection is empty or only contains `nil`."
  [coll]
  (first (filter some? coll)))

(defn weights-to-probs
  "Normalizes the numeric values of `m` into a proportion of the total.
  Returns a map with values that sum to 1."
  [m]
  (let [total (reduce + (vals m))]
    (->> m
         (map (fn [[k w]] [k (/ w total)]))
         (into {}))))

(defn rand-weighted
  [m]
  (let [weights (reductions + (vals m))
        total (last weights)
        choices (map vector (keys m) weights)]
    (let [choice (rand total)]
      (loop [[[c w] & more] choices]
        (when w
          (if (< choice w)
            c
            (recur more)))))))


(defn safe-rand-nth
  [coll]
  (if (empty? coll)
    nil
    (rand-nth coll)))

(defn enhance
  ([m k f]
   (assoc m k (f m)))
  ([m k f & kfs]
   (->> kfs
        (partition 2 2)
        (cons [k f])
        (reduce (fn [m [k f]] (enhance m k f)) m))))

(defn enhance-missing
  ([m k f]
   (if (contains? m k) m (enhance m k f)))
  ([m k f & kfs]
   (->> kfs
        (partition 2 2)
        (cons [k f])
        (reduce (fn [m [k f]] (enhance-missing m k f)) m))))