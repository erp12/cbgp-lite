(ns erp12.cbgp-lite.utils)

(defn first-non-nil
  "Returns the first non-nil values from the collection, or returns `nil` if
  the collection is empty or only contains `nil`."
  [coll]
  (first (filter some? coll)))

(defn rand-weighted
  [m]
  (let [weights   (reductions + (vals m))
        total   (last weights)
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
