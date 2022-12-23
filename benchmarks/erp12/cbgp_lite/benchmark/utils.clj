(ns erp12.cbgp-lite.benchmark.utils)

(defn read-problem
  [{:keys [suite-ns problem] :as config}]
  (require suite-ns)
  (let [suite-ns (find-ns suite-ns)
        suite-problems ((ns-resolve suite-ns 'problems) config)
        problem-info (get suite-problems (name problem))
        read-cases (ns-resolve suite-ns 'read-cases)]
    (merge config (read-cases config) problem-info)))

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

(defn round
  "Round a double to the given precision (number of significant digits)"
  [precision n]
  (if (nil? n)
    nil
    (let [factor (Math/pow 10 precision)]
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
    (+' (apply +' (map (fn [cor res]
                         (absolute-distance cor res))
                       expected
                       actual))
        (*' 1000 (abs (- (count expected) (count actual)))))))