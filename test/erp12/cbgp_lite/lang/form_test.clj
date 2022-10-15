(ns erp12.cbgp-lite.lang.form-test
  (:require [clojure.test :refer [deftest is]]
            [erp12.cbgp-lite.lang.form :as form]))

(deftest ast->form-test
  ;(is (= (form/ast->form {:op   :invoke
  ;                        :fn   {:op :var :var '+}
  ;                        :args [{:op :const :val 100}
  ;                               {:op :local :name 'x}]})
  ;       '(+ 100 x)))
  ;(is (= (form/ast->form {:op       :let
  ;                        :bindings [{:op   :binding
  ;                                    :name 'f
  ;                                    :init {:op      :fn
  ;                                           :methods [{:op     :fn-method
  ;                                                      :params [{:op :binding :name 'a}]
  ;                                                      :body   {:op :local :name 'a}}]}}]
  ;                        :body     {:op   :invoke
  ;                                   :fn   {:op :local :name 'f}
  ;                                   :args [{:op :const :val "!"}]}})
  ;       '(let [f (fn [a] a)]
  ;          (f "!"))))
  (is (= (form/ast->form {:op   :invoke
                          :fn   {:op :var :var 'mapv}
                          :args [{:op      :fn
                                  :methods [{:op     :fn-method
                                             :params [{:op :binding :name 'a}]
                                             :body   {:op   :invoke
                                                      :fn   {:op :var :var 'inc}
                                                      :args [{:op :local :name 'a}]}}]}
                                 {:op :const :val [1 2 3]}]})
         '(mapv (fn [a] (inc a)) [1 2 3])))

  )