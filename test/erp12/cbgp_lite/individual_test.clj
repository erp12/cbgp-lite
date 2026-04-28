(ns erp12.cbgp-lite.individual-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.cbgp-lite.individual :as i]
            [erp12.cbgp-lite.genome :as g]
            [erp12.cbgp-lite.program.types :as t]
            [erp12.cbgp-lite.program.lib :as lib]))

(defn absolute-dist [a b] (Math/abs (- a b)))

(deftest with-out-and-stdout-test
  (is (= {:output 2 :std-out "Hi\n"}
         (i/with-out-and-stdout (do (println "Hi") (inc 1))))))

(deftest errors-for-case-test
  (is (= [2 1]
         (i/errors-for-case {:actual   {:output 1 :std-out "B"}
                             :expected {:output -1 :std-out "A"}
                             :penalty     100
                             :loss-fns    [absolute-dist]})))
  (is (= [100 1]
         (i/errors-for-case {:actual   {:exception :FAIL :std-out "B"}
                             :expected {:output -1 :std-out "A"}
                             :penalty     100
                             :loss-fns    [absolute-dist]}))))

(deftest evaluate-func-test
  (let [opts {:cases    [{:inputs [1 2] :output 3 :std-out "2"}
                         {:inputs [-1 1] :output 0 :std-out "1"}
                         {:inputs [0 0] :output 0 :std-out "0"}]
              :loss-fns [absolute-dist]
              :penalty  100}]
    (is (= (i/evaluate-func (assoc opts :func #(do (print %2) (+ %1 %2))))
           {:behavior (list {:output 3 :std-out "2"}
                            {:output 0 :std-out "1"}
                            {:output 0 :std-out "0"})
            :cases-used 3
            :errors [0 0 0 0 0 0]
            :solution? true
            :total-error 0
            :exception nil
            :exceeded-mem false}))
    (is (= (i/evaluate-func (assoc opts :func #(do (println %2) (* %1 %2))))
           {:behavior (list {:output  2
                             :std-out "2\n"}
                            {:output  -1
                             :std-out "1\n"}
                            {:output  0
                             :std-out "0\n"})
            :cases-used  3
            :errors [1 1 1 1 0 1]
            :solution? false
            :total-error 5
            :exception nil
            :exceeded-mem false}))))

(deftest make-genome-evaluator-test
  (let [evaluator (i/make-genome-evaluator {:input-symbols ['input1 'input2]
                                            :input-types [t/FLOAT t/INT]
                                            :output-type t/FLOAT
                                            :loss-fns [absolute-dist]
                                            :penalty 1000})]
    (is (= (dissoc (evaluator (list (g/->Lit 1.0 t/FLOAT))
                              {:cases [{:inputs [1.5 2] :output 3.5}]})
                   :func)
           {:behavior '({:output 1.0 :std-out ""})
            :code 1.0
            :errors [2.5]
            :push [(g/->Lit 1.0 t/FLOAT)]
            :total-error 2.5
            :cases-used 1
            :solution? false
            :exception nil
            :exceeded-mem false}))))

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
    (let [evaluator (i/make-genome-evaluator {:input-symbols ['input1 'input2]
                                              :input-types [t/FLOAT t/INT]
                                              :output-type t/FLOAT
                                              :loss-fns [absolute-dist]
                                              :penalty 1000
                                              :cases  [{:inputs [1.5 2] :output 3.5}
                                                       {:inputs [0.0 0] :output 0.0}
                                                       {:inputs [-1.0 1] :output 0.0}]})
          genome (list (g/->Var 'input1)
                       (g/->Var 'input2)
                       (g/->Var `float)
                       (g/->App)
                       (g/->Var `lib/float-add)
                       (g/->App))]
      (is (= {:behavior '({:output 3.5 :std-out ""}
                          {:output 0.0 :std-out ""}
                          {:output 0.0 :std-out ""})
              :code `(lib/float-add (float ~'input2) ~'input1)
              :errors [0.0 0.0 0.0]
              :genome genome
              :push (vec genome)
              :total-error 0.0
              :cases-used 3
              :solution? true
              :exception nil
              :exceeded-mem false}
             (dissoc (i/simplify {:individual (assoc (evaluator genome {}) :genome genome)
                                  :simplification-steps 3
                                  :evaluator evaluator})
                     :func))))))
