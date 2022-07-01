(ns erp12.cbgp-lite.benchmark.ga
  (:require [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.gp :as gp]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.ga-clj.toolbox :as tb]
            [taoensso.timbre :as log]))

(def config
  {:n-train              100
   :n-test               300
   :population-size      1000
   :max-generations      300
   :umad-rate            0.1
   :min-genome-size      50
   :max-genome-size      250
   :gene-distribution    {:close         0.1
                          :var           0.2
                          :local         0.15
                          :lit           0.15
                          :lit-generator 0.1
                          :abstraction   0.15
                          :apply         0.15}
   :penalty              1e5
   :simplification-steps 2000
   :downsample-rate      1.0})

(defn make-breed
  [opts]
  (let [select (tb/make-lexicase-selection opts)
        mutate (tb/make-size-neutral-umad opts)]
    (fn [generation]
      (->> (select generation) :genome mutate))))

(defn run
  [{:keys [suite-ns problem] :as opts}]
  ;(doseq [[k v] opts]
  ;  (println k ":" (class k) " -> " v ":" (class v)))
  (require suite-ns)
  (let [suite-ns (find-ns suite-ns)
        problem-map ((ns-resolve suite-ns 'problems) config)
        info (problem-map (name problem))
        read-cases (ns-resolve suite-ns 'read-cases)
        {:keys [train test]} (read-cases (merge config opts))
        genetic-source (bu/genetic-source (assoc info :gene-distribution (:gene-distribution config)))
        breed (make-breed {:rate 0.1 :genetic-source genetic-source})
        {:keys [best result]} (gp/run (merge config
                                             info
                                             {:vars           (set (keys (lib/lib-for-types (bu/problem-types info))))
                                              :cases          train
                                              :breed          breed
                                              :genetic-source genetic-source}))
        {:keys [total-error]} (gp/evaluate-code {:code        (:code best)
                                                 :arg-symbols (vec (sort (keys (:input->type info))))
                                                 :cases       test
                                                 :loss-fns    (:loss-fns info)
                                                 :penalty     (:penalty config)})]
    (log/info "BEST INDIVIDUAL" best)
    (log/info "BEST CODE" (reverse (into '() (:code best))))
    (if (= :solution-found result)
      (do
        (log/info "SOLUTION FOUND")
        (if (zero? total-error)
          (log/info "SOLUTION GENERALIZED")
          (log/info "SOLUTION FAILED TO GENERALIZE")))
      (log/info "SOLUTION NOT FOUND"))
    (:func best)))

;(comment
;
;  (run {:suite-ns 'erp12.cbgp-lite.benchmark.suite.psb1
;        :data-dir "data/program-synthesis-benchmark-datasets/datasets"
;        :problem  'replace-space-with-newline})
;
;  )