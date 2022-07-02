(ns erp12.cbgp-lite.benchmark.utils
  (:require [taoensso.timbre :as log]))

(defn read-problem
  [{:keys [suite-ns problem config]}]
  (require suite-ns)
  (let [suite-ns (find-ns suite-ns)
        suite-problems ((ns-resolve suite-ns 'problems) config)
        problem-info (get suite-problems (name problem))
        read-cases (ns-resolve suite-ns 'read-cases)
        task (merge config (read-cases config) problem-info)]
    task))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC Generators

(defn rand-bool
  []
  (> (rand) 0.5))

(defn int-generator
  [magnitude]
  #(- (rand-int (inc (* 2 magnitude))) magnitude))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loss Function Utils

(defn round
  "Round a double to the given precision (number of significant digits)"
  [precision n]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* n factor)) factor)))

(defn absolute-distance
  [actual expected]
  (Math/abs (- actual expected)))
