(ns erp12.cbgp-lite.task
  (:require [clojure.set :as set]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.search.pluhsy :as pl]
            [erp12.cbgp-lite.utils :as u]))

(defn arg-symbols
  [{:keys [input->type]}]
  (vec (sort (keys input->type))))

(defn task-types
  [{:keys [input->type ret-type other-types] :or {other-types #{}}}]
  (set/union (set (vals input->type))
             #{ret-type}
             (set other-types)))

(defn vars-for-types
  [types]
  (set (keys (lib/lib-for-types types))))

(defn type-environment
  [{:keys [input->type vars]}]
  (->> lib/type-env
       (filter (fn [[symb _]] (contains? vars symb)))
       (merge input->type)))

(def default-gene-distribution
  ;; @todo Calibrate by analyzing real code.
  {:var           0.2
   :local         0.2
   :lit           0.2
   :lit-generator 0.1
   :apply         0.2
   :fn            0.025
   :let           0.025
   :close         0.05})

(defn default-genetic-source
  [{:keys [types vars extra-genes]}]
  (pl/make-genetic-source
    (pl/prob-by-gene-kind (concat (map (fn [v] {:gene :var :name v}) vars)
                                  ;; Task-specific genes
                                  extra-genes
                                  ;; 1-arg functions
                                  (for [arg types ret types]
                                    {:gene :fn :arg-types [arg] :ret-type ret})
                                  ;; 2-arg functions
                                  (for [arg1 types
                                        arg2 types
                                        ret types]
                                    {:gene :fn :arg-types [arg1 arg2] :ret-type ret})
                                  ;; Always used genes
                                  [{:gene :local}
                                   {:gene :apply}
                                   {:gene :let}
                                   {:gene :close}])
                          default-gene-distribution)))

(defn enhance-task
  [opts]
  (-> opts
      (assoc :dealiases lib/dealiases)
      (u/enhance
        ;; The size of an individual's error vector
        :num-errors (fn [{:keys [train loss-fns stdout-key]}]
                      (+ (* (count train) (count loss-fns))
                         (if (nil? stdout-key) 0 (count train))))
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
        :genetic-source default-genetic-source
        ;; Derive a function for generating genomes.
        :genome-factory (fn [opts] #(pl/random-plushy-genome opts)))))