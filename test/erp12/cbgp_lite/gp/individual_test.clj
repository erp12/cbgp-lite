(ns erp12.cbgp-lite.gp.individual-test
  (:require [clojure.test :refer [deftest testing is]]
            [erp12.cbgp-lite.gp.individual :refer [simplify]]
            [erp12.cbgp-lite.gp :refer [make-individual-factory]]))

(deftest simplify-test
  (let [opts {:simplification-steps 100
              :individual-factory   (fn [gn _]
                                      ;; Look for the smallest genome that adds to 10.
                                      {:total-error (+ (Math/abs (- 10 (apply + gn)))
                                                       (count gn)
                                                       (if (empty? gn) 1000 0))})}]
    (doseq [_ (range 10)]
      (is (= {:genome [10] :total-error 1}
             (simplify (assoc opts :individual {:genome      [10 10 10 10]
                                                :total-error (+ 30 4)}))))))
  (testing "number-io"
    (let [factory (make-individual-factory {:input->type {'input1 float?
                                                          'input2 int?}
                                            :return-type float?
                                            :vars        #{'float 'float-add}
                                            :loss-fn     #(Math/abs (- %1 %2))
                                            :penalty     1000})
          opts {:simplification-steps 1000
                :individual-factory   factory}
          cases [{:inputs [1.5 2] :output 3.5}
                 {:inputs [0.0 0] :output 0.0}]]
      (is (= {:behavior    '(3.5 0.0)
              :code        '(+ (float input2) input1)
              :errors      [0.0 0.0]
              :genome      [[:var 'input1] [:var 'input2] [:var 'float] :apply [:var 'float-add] :apply]
              :push        [[:var 'input1] [:var 'input2] [:var 'float] :apply [:var 'float-add] :apply]
              :total-error 0.0}
             (let [gn [[:var 'input1]
                       [:var 'input2]
                       [:var 'float]
                       :apply
                       [:var 'float-add]
                       :apply]]
               (-> {:individual (assoc (factory gn cases) :genome gn)
                    :context    {:cases cases}}
                   (merge opts)
                   simplify
                   (dissoc :func))))))))
