(ns erp12.cbgp-lite.gp-test
  (:require [clojure.test :refer [deftest testing is]]
            [erp12.cbgp-lite.gp :refer [make-individual-factory]]))

(deftest individual-factory-test
  (let [factory (make-individual-factory {:input->type {'input1 float?
                                                        'input2 int?}
                                          :return-type float?
                                          :vars        #{'+ 'float}
                                          :loss-fns    [#(Math/abs (- %1 %2))]
                                          :penalty     1000})]
    (is (= {:behavior    '({:output nil :std-out ""})
            :code        nil
            :errors      [1000]
            :push        []
            :total-error 1000}
           (dissoc (factory '() {:cases [{:inputs [1.5 2] :output 3.5}]})
                   :func)))))
