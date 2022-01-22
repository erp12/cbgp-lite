(ns erp12.cbgp-lite.gp.individual)

(defn simplify
  [{:keys [individual simplification-steps individual-factory context]}]
  (reduce (fn [{:keys [genome total-error] :as best} _]
            (let [new-gn (vec (random-sample (rand) genome))
                  new-indiv (assoc (individual-factory new-gn context) :genome new-gn)]
              ;(println genome new-gn)
              (if (<= (:total-error new-indiv) total-error)
                new-indiv
                best)))
          individual
          (range simplification-steps)))
