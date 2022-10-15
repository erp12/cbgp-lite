(ns erp12.cbgp-lite.lang.schema-test
  (:require [clojure.test :refer :all]
            [erp12.cbgp-lite.lang.schema :as sch]))

(deftest occurs?-test
  (is (sch/occurs? 'a {:op :local :name 'a}))
  (is (sch/occurs? {:type :s-var :sym 'A}
                   {:type   :=>
                    :input  {:type     :cat
                             :children [{:type :s-var :sym 'A}]}
                    :output {:type 'int?}}))
  (is (not (sch/occurs? 'x {:op :var :var '+})))
  (is (not (sch/occurs? {:type :s-var :sym 'A}
                        {:type   :=>
                         :input  {:type     :cat
                                  :children [{:type :s-var :sym 'B}]}
                         :output {:type 'int?}}))))
