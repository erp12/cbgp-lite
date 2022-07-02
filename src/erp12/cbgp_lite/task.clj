(ns erp12.cbgp-lite.task
  (:require [clojure.set :as set]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.search.pluhsy :as pl]
            [erp12.cbgp-lite.utils :as u]
            [taoensso.timbre :as log]))

(defn arg-symbols
  [{:keys [input->type]}]
  (vec (sort (keys input->type))))

(defn task-types
  [{:keys [input->type return-type other-types] :or {other-types #{}}}]
  (set/union (set (vals input->type)) #{return-type} (set other-types)))

(defn vars-for-types
  [types]
  (set (keys (lib/lib-for-types types))))

(defn type-environment
  [{:keys [input->type vars]}]
  (->> lib/library
       (filter (fn [[symb _]] (contains? vars symb)))
       (merge input->type)
       (mapv (fn [[symb typ]] [:= symb typ]))))

(def default-gene-distribution
  {:close         0.1
   :var           0.2
   :local         0.15
   :lit           0.15
   :lit-generator 0.1
   :abstraction   0.15
   :apply         0.15})

(defn genetic-source
  [{:keys [types vars literals lit-generators]}]
  (let [vars (vec vars)
        abstraction (vec (concat [:let [:fn]]
                                 ;; 1-arg functions
                                 (map (partial vector :fn) types)
                                 ;; 2-arg functions
                                 (for [arg1 types
                                       arg2 types]
                                   [:fn arg1 arg2])))]
    (fn []
      (pl/random-gene {:vars              vars
                       :lits              literals
                       :lit-generators    lit-generators
                       :abstraction       abstraction
                       :gene-distribution default-gene-distribution}))))

(defn enhance-task
  [opts]
  (-> opts
      (assoc :dealiases lib/dealiases)
      (u/enhance
        ;; Create a sequence of program argument symbols
        :arg-symbols arg-symbols
        ;; Find all types related to the task
        :types task-types
        ;; Find the set of all variables that leverage to task's types.
        ;; Includes generic functions.
        :vars (fn [{:keys [types]}] (vars-for-types types))
        ;; Derive the full type-environment used to compile programs from genomes.
        :type-env type-environment
        ;; Derive the genetic source.
        :genetic-source genetic-source
        ;; Derive a function for generating genomes.
        :genome-factory (fn [opts] #(pl/random-plushy-genome opts)))))