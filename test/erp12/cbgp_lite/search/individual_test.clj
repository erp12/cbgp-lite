(ns erp12.cbgp-lite.search.individual-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.cbgp-lite.search.individual :as i]
            [erp12.cbgp-lite.task :as task]
            [erp12.cbgp-lite.utils :as u]))

(defn absolute-dist [a b] (Math/abs (- a b)))

(deftest with-out-and-stdout-test
  (is (= {:output 2 :std-out "Hi\n"}
         (i/with-out-and-stdout (do (println "Hi") (inc 1))))))

(deftest compute-errors-on-case-test
  (is (= [2 1]
         (i/errors-for-case {:case        {:output -1 :std-out "A"}
                             :prog-output {:output 1 :std-out "B"}
                             :penalty     100
                             :loss-fns    [absolute-dist]}))))

(deftest evaluate-until-first-failure-test
  (let [opts {:cases    [{:inputs [1 2] :output 3 :std-out "2"}
                         {:inputs [-1 1] :output 0 :std-out "1"}
                         {:inputs [0 0] :output 0 :std-out "0"}]
              :loss-fns [absolute-dist]
              :penalty  100}]
    (is (= {:cases-used 3 :solution? true}
           (i/evaluate-until-first-failure (assoc opts :func #(do (print %2) (+ %1 %2))))))
    (is (= {:cases-used 1}
           (i/evaluate-until-first-failure (assoc opts :func #(do (print %2) (* %1 %2))))))
    (is (instance? ArithmeticException
                   (:exception (i/evaluate-until-first-failure (assoc opts :func #(do (print %2) (/ %1 0)))))))))

(deftest evaluate-full-behavior-test
  (let [opts {:cases    [{:inputs [1 2] :output 3 :std-out "2"}
                         {:inputs [-1 1] :output 0 :std-out "1"}
                         {:inputs [0 0] :output 0 :std-out "0"}]
              :loss-fns [absolute-dist]
              :penalty  100}]
    (is (= (i/evaluate-full-behavior (assoc opts :func #(do (print %2) (+ %1 %2))))
           {:behavior    (list {:output 3 :std-out "2"}
                               {:output 0 :std-out "1"}
                               {:output 0 :std-out "0"})
            :cases-used  3
            :errors      [0 0 0 0 0 0]
            :solution?   true
            :total-error 0
            :exception   nil}))
    (is (= (i/evaluate-full-behavior (assoc opts :func #(do (println %2) (* %1 %2))))
           {:behavior    (list {:output 2 :std-out "2\n"}
                               {:output -1 :std-out "1\n"}
                               {:output 0 :std-out "0\n"})
            :cases-used  3
            :errors      [1 1 1 1 0 1]
            :solution?   false
            :total-error 5
            :exception   nil}))))

(deftest make-evaluator-test
  (let [evaluator (i/make-evaluator (-> {:input->type {'input1 {:type 'double?}
                                                       'input2 {:type 'int?}}
                                         :ret-type    {:type 'double?}
                                         :loss-fns    [absolute-dist]
                                         :penalty     1000
                                         :evaluate-fn i/evaluate-full-behavior}
                                        (u/enhance :arg-symbols task/arg-symbols
                                                   :type-env task/type-environment)))]
    (is (= (dissoc (evaluator (list {:gene :lit
                                     :val  1.0
                                     :type {:type 'double?}})
                              {:cases [{:inputs [1.5 2] :output 3.5}]})
                   :func)
           {:behavior    '({:output 1.0 :std-out ""})
            :code        1.0
            :errors      [2.5]
            :push        [{:gene :lit :val 1.0 :type {:type 'double?}}]
            :total-error 2.5
            :cases-used  1
            :solution?   false
            :exception   nil}))
    ;(is (= (dissoc (factory (list)
    ;                        {:cases [{:inputs [1.5 2] :output 3.5}]})
    ;               :func)
    ;       {:behavior    nil
    ;        :code        nil
    ;        :errors      [1000 1000]
    ;        :push        (list)
    ;        :total-error 2000
    ;        :cases-used  0}))
    ))

(deftest simplify-test
  (let [opts {:simplification-steps 100
              :evaluator            (fn [gn _]
                                      ;; Look for the smallest genome that adds to 10.
                                      {:total-error (+ (Math/abs (- 10 (apply + gn)))
                                                       (count gn)
                                                       (if (empty? gn) 1000 0))})}]
    (is (= {:genome [10] :total-error 1}
           (i/simplify (assoc opts
                              :individual {:genome      [10 10 10 10]
                                           :total-error (+ 30 4)})))))
  (testing "number-io"
    (let [evaluator (i/make-evaluator (task/enhance-task {:input->type {'input1 {:type 'double?}
                                                                        'input2 {:type 'int?}}
                                                          :ret-type    {:type 'double?}
                                                          :loss-fns    [absolute-dist]
                                                          :penalty     1000
                                                          :evaluate-fn i/evaluate-full-behavior}))
          cases [{:inputs [1.5 2] :output 3.5}
                 {:inputs [0.0 0] :output 0.0}
                 {:inputs [-1.0 1] :output 0.0}]]
      (is (= {:behavior    '({:output 3.5 :std-out ""}
                             {:output 0.0 :std-out ""}
                             {:output 0.0 :std-out ""})
              :code        '(+ (double input2) input1)
              :errors      [0.0 0.0 0.0]
              :genome      (list {:gene :var :name 'input1}
                                 {:gene :var :name 'input2}
                                 {:gene :var :name 'double}
                                 {:gene :apply}
                                 {:gene :var :name 'double-add}
                                 {:gene :apply})
              :push        [{:gene :var :name 'input1}
                            {:gene :var :name 'input2}
                            {:gene :var :name 'double}
                            {:gene :apply}
                            {:gene :var :name 'double-add}
                            {:gene :apply}]
              :total-error 0.0
              :cases-used  3
              :solution?   true
              :exception   nil}
             (let [gn (list {:gene :var :name 'input1}
                            {:gene :var :name 'input2}
                            {:gene :var :name 'double}
                            {:gene :apply}
                            {:gene :var :name 'double-add}
                            {:gene :apply})]
               (-> {:individual           (assoc (evaluator gn {:cases cases}) :genome gn)
                    :context              {:cases cases}
                    :simplification-steps 3
                    :evaluator            evaluator}
                   i/simplify
                   (dissoc :func))))))))
