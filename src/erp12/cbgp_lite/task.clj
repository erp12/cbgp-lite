(ns erp12.cbgp-lite.task
  (:require [clojure.set :as set]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.schema :as schema]
            [erp12.cbgp-lite.search.plushy :as pl]
            [erp12.cbgp-lite.utils :as u]))

;; @TODO Move to `benchmarks/`

(defn arg-symbols
  [{:keys [input->type]}]
  (vec (sort (keys input->type))))

(defn task-type-ctors
  [{:keys [input->type ret-type other-type-ctors] :or {other-type-ctors #{}}}]
  (->> (schema/schema-terms {:type :=>
                             :input {:type :cat
                                     :children (vec (vals input->type))}
                             :output ret-type})
       (set/union (set other-type-ctors))
       (remove #{:cat :s-var :scheme})
       (set)))

(defn type-environment
  [{:keys [input->type type-ctors]}]
  (merge (lib/lib-for-type-ctors type-ctors)
         input->type))

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
  [{:keys [vars extra-genes]}]
  (pl/make-genetic-source
   (pl/prob-by-gene-kind (concat (map (fn [v] {:gene :var :name v}) vars)
                                 ;; Task-specific genes
                                 extra-genes
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
       :type-ctors task-type-ctors
       ;; Derive the full type-environment used to compile programs from genomes.
       :type-env type-environment
       ;; Find the set of all variables that leverage to task's types.
       ;; Includes generic functions.
       :vars (fn [{:keys [type-env]}] (set (keys type-env)))
        ;; Derive the genetic source.
       :genetic-source default-genetic-source
        ;; Derive a function for generating genomes.
       :genome-factory (fn [opts] #(pl/random-plushy-genome opts)))))