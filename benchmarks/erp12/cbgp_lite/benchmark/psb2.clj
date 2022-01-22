;(ns erp12.cbgp-lite.benchmark.psb2
;  (:gen-class)
;  (:require [psb2.core :as psb2]
;            [erp12.ga-clj.toolbox :as tb]
;            [erp12.cbgp-lite.gp :as gp]
;            [erp12.cbgp-lite.utils :as u]
;            [erp12.cbgp-lite.benchmark.psb-utils :as pu]))
;
;(def config
;  {:n-train           200
;   :n-test            2000
;   :population-size   1000
;   :max-generations   300
;   :umad-rate         0.1
;   :min-genome-size   50
;   :max-genome-size   250
;   :gene-distribution {:open-close    0.2
;                       :input         0.2
;                       :var           0.2
;                       :lit           0.2
;                       :lit-generator 0.1
;                       :abstraction   0.1}})
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration
;;; It isn't easy to swap functions at the CLI, so these are hard-coded.
;
;(defn make-breed
;  [opts]
;  (let [select (tb/make-lexicase-selection opts)
;        mutate (tb/make-size-neutral-umad opts)]
;    (fn [{:keys [population]}]
;      (->> (repeatedly 2 #(select population))
;           (map :genome)
;           mutate))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(def problem-info
;  {"gcd" {:input->type    {'input1 int? 'input2 int?}
;          :return-type    int?
;          :other-types    [boolean?]
;          :literals       []
;          :lit-generators [#(- (rand-int 21) 10)]
;          :loss-fn        #(Math/abs (- %1 %2))}
;   })
;
;(defn run
;  [{:keys [problem data-path]}]
;  {:pre [(contains? problem-info problem)]}
;  (let [info (problem-info problem)
;        {:keys [train test]} (psb2/fetch-examples data-path problem (:n-train config) (:n-test config))
;        train-cases (map pu/reshape-case train)
;        genetic-source (pu/genetic-source (assoc info :gene-distribution (:gene-distribution config)))
;        breed (make-breed {:rate           0.1
;                           :genetic-source genetic-source})
;        {:keys [best]} (gp/run (merge config
;                                      info
;                                      {:cases          train-cases
;                                       :breed          breed
;                                       :genetic-source genetic-source}))
;        ;; @todo Genome simplification
;        test-cases (map pu/reshape-case test)
;        y-pred (map #(apply (:func best) (:inputs %)) test-cases)
;        y-true (map :output1 test-cases)
;        test-errors (map (:loss info) y-pred y-true)
;        test-error (apply + test-errors)]
;    ;; @todo Write report.
;    ))
