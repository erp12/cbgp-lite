(ns erp12.cbgp-lite.gp
  (:require [erp12.ga-clj.generational :refer [evolve]]
            [erp12.ga-clj.toolbox :as tb]
            [erp12.cbgp-lite.gp.pluhsy :as pl]
            [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.lang.lib :as lib]))

(def tournament
  (tb/make-tournament-selection {:by :error :size 7}))

;; @todo This is just a sketch. Tweak config and/or logic as needed.
;; @todo Parameterize parent selection and variation?
(defn run
  [{:keys [input->type return-type cases loss-fn max-generations population-size] :as opts}]
  (let [arg-symbols (vec (keys input->type))]
    (evolve
      (merge {:genome-factory  #(pl/random-plushy-genome {}) ;; @todo <--
              :genome->phenome (fn [gn]
                                 (let [;; Get Push code from the genome.
                                       push (pl/plushy->push gn)
                                       ;; Compile the Push into a Clojure form that accepts and returns the
                                       ;; correct types.
                                       code (c/push->clj {:push          push
                                                          :inputs        arg-symbols
                                                          :ret-type      return-type
                                                          :type-env      (vec (concat lib/environment
                                                                                      (map #(vec (cons := %)) input->type)))
                                                          :alias->symbol lib/alias->symbol})
                                       ;; Create a Clojure function with the compiled code.
                                       func (c/synth-fn arg-symbols code)
                                       ;; Call the function on each training case.
                                       behavior (->> cases
                                                     (map :inputs)
                                                     (mapv #(apply func %)))
                                       ;; Compute the error on each case.
                                       errors (->> cases
                                                   (map :output)
                                                   (map loss-fn behavior))]
                                   {:push        push
                                    :code        code
                                    :func        func
                                    :behavior    behavior
                                    :errors      errors
                                    :total-error (apply + errors)}))
              :breed           (fn [population]
                                 (->> (repeatedly 2 #(tournament population))
                                      (map :genome)
                                      tb/uniform-crossover
                                      tb/swap-2-genes))
              :phenome-cmp     (comparator #(< (:total-error %1) (:total-error %2)))
              :stop-fn         (fn [{:keys [generation best]}]
                                 (cond
                                   (= (:error best) 0) :solution-found
                                   (= generation max-generations) :max-generation-reached))
              :population-size population-size}
             opts))))
