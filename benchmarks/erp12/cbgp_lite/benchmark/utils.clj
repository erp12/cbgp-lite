(ns erp12.cbgp-lite.benchmark.utils
  (:require [clojure.set :as set]
            [erp12.cbgp-lite.gp.pluhsy :as pl]
            [erp12.cbgp-lite.lang.lib :as lib]))

(defn problem-types
  [{:keys [input->type return-type other-types]}]
  (set/union (set (vals input->type)) #{return-type} other-types))

(defn genetic-source
  [{:keys [literals lit-generators gene-distribution] :as info}]
  (let [types (problem-types info)
        opts {:vars              (vec (keys (lib/lib-for-types types)))
              :lits              literals
              :lit-generators    lit-generators
              :abstraction       (vec (concat [:let [:fn]]
                                              ;; 1-arg functions
                                              (map (partial vector :fn) types)
                                              ;; 2-arg functions
                                              (for [arg1 types
                                                    arg2 types]
                                                [:fn arg1 arg2])))
              :gene-distribution gene-distribution}]
    #(pl/random-gene opts)))


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
