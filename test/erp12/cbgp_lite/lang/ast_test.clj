(ns erp12.cbgp-lite.lang.ast-test
  (:require [clojure.test :refer [deftest is]]
            [erp12.cbgp-lite.lang.ast :as a]))

(deftest ast-size-test
  (is (= (a/ast-size {:op :var :var #'+}) 1))
  (is (= (a/ast-size {:op :const :val 100}) 1))
  (is (= 3
         (a/ast-size {:op   :invoke
                      :fn   {:op :local :name 'f}
                      :args [{:op :local :name 'x}
                             {:op :local :name 'y}]})))

  (is (= 6
         ;; AST for:
         ;;(fn [x] (do (println "Hello World") x))
         (a/ast-size {:op      :fn
                      :methods [{:op     :fn-method
                                 :params [{:op :binding :name 'x}]
                                 :body   {:op   :invoke
                                          :fn   {:op :var :var 'do}
                                          :args [{:op   :invoke
                                                  :fn   {:op :var :var #'println}
                                                  :args [{:op :const :val "Hello World"}]}
                                                 {:op :local :name 'x}]}}]}))))

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
  (is (= (a/ast->form {:op   :invoke
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