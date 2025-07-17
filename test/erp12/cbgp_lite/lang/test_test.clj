(ns erp12.cbgp-lite.lang.test-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.walk :as w]
            [erp12.cbgp-lite.lang.ast :as a]
            [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.schema :as schema]
            [mb.hawk.core]
            [meander.epsilon :as m]
            [erp12.cbgp-lite.lang.ast :as ast]))

(def verbose true)

#_(deftest mapcat-vector-test-test
    (testing "mapcat Vector"
      (let [{::c/keys [ast type]} (:ast (c/push->ast
                                         {:push      [{:gene :lit :val [6 5 9] :type {:type :vector :child {:type 'int?}}}
                                                      {:gene :lit :val [[1 2 3] [55 6 98]] :type {:type :vector :child {:type :vector :child {:type 'int?}}}} 
                                                      {:gene :var :name 'inc} 
                                                      {:gene :var :name `lib/mapcat'}
                                                      {:gene :apply}]
                                          :locals    []
                                          :ret-type  {:type :vector :child {:type 'int?}}
                                          :type-env  lib/type-env
                                          :dealiases lib/dealiases}))
            _ (is (= :vector (:type type)))
            _ (println "REAL-AST: " ast)
            form (a/ast->form ast)
            _ (println "FORM: " form)
            func (eval `(fn [] ~form))]
        (is (= [3 2 1 98 6 55] (func))))))

#_(deftest temp
  (testing "mapv-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val {1 4, 98 3} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                    {:gene :lit :val {40 \h, 50 \e, 200 \p} :type {:type :map-of :key {:type 'int?} :value {:type 'char?}}}
                                                    {:gene :fn :arg-types [(lib/tuple-of lib/INT lib/CHAR)] :ret-type (lib/tuple-of lib/INT lib/INT)}
                                                    [{:gene :local :idx 0}
                                                     {:gene :var :name 'right}
                                                     {:gene :apply}
                                                     {:gene :var :name 'int}
                                                     {:gene :apply}]
                                                    {:gene :var :name 'mapv}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child (lib/tuple-of lib/INT lib/INT)} ;;[!!]
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))]
      (is (= {50 \h, 60 \e, 210 \p} (func))))))

;; (deftest Vector-First-first
;;   (testing "Vector first alt"
;;     (let [{::c/keys [ast type]} (:ast (c/push->ast
;;                                        {:push      [{:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}} 
;;                                                     {:gene :lit :val "hello" :type {:type 'string?}}
;;                                                     {:gene :var :name 'first}
;;                                                     {:gene :apply}]
;;                                         :locals    []
;;                                         :ret-type  {:type :s-var}
;;                                         :type-env  lib/type-env
;;                                         :dealiases lib/dealiases}))
;;           _ (is (= type {:type 'char?}))
;;           _ (println "REAL-AST: " ast)
;;           form (a/ast->form ast)
;;           _ (println "FORM: " form)
;;           func (eval `(fn [] ~form))]
;;       (is (= false (func))))))

;; (deftest String-First-first
;;   (testing "String first alt"
;;     (let [{::c/keys [ast type]} (:ast (c/push->ast
;;                                        {:push      [{:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
;;                                                     {:gene :lit :val "hello" :type {:type 'string?}}
;;                                                     {:gene :var :name 'first}
;;                                                     {:gene :apply}]
;;                                         :locals    []
;;                                         :ret-type  {:type :s-var}
;;                                         :type-env  (assoc lib/type-env
;;                                                           'first {:type :overloaded 
;;                                                                   :alternatives [(lib/fn-of [lib/STRING] lib/CHAR)
;;                                                                                  (lib/scheme (lib/fn-of [(lib/vector-of (lib/s-var 'a))] (lib/s-var 'a)))]})
;;                                         :dealiases lib/dealiases}))
;;           _ (is (= type {:type 'int?}))
;;           _ (println "REAL-AST: " ast)
;;           form (a/ast->form ast)
;;           _ (println "FORM: " form)
;;           func (eval `(fn [] ~form))]
;;       (is (= false (func))))))



;; ()
;; (deftest temp
;;   (is (= true (eval (a/ast->form (::c/ast (:ast (c/push->ast
;;        {:push [{:gene :lit :val [1 \h] :type (lib/tuple-of lib/INT lib/CHAR)}
;;                {:gene :var :name 'left}
;;                {:gene :apply} 
;;                {:gene :var :name 'right}
;;                {:gene :apply}
;;                {:gene :var :name 'int}
;;                {:gene :apply}
;;                {:gene :var :name `lib/<'}
;;                {:gene :apply}
;;                ]
;;         :locals    ['in1]
;;         :ret-type  {:type 'boolean?} ;;[!!]
;;         :type-env  (assoc lib/type-env
;;                           'in1 {:type :map-of :key {:type 'int?} :value {:type 'int?}})
;;         :dealiases lib/dealiases}))))))))

;; (eval (a/ast->form (::c/ast (:ast (c/push->ast
;;                                    {:push [{:gene :lit :val [1 \h] :type (lib/tuple-of lib/INT lib/CHAR)}
;;                                            {:gene :var :name 'left}
;;                                            {:gene :apply}
;;                                            {:gene :lit :val [1 \h] :type (lib/tuple-of lib/INT lib/CHAR)}
;;                                            {:gene :var :name 'right}
;;                                            {:gene :apply}
;;                                            {:gene :var :name 'int}
;;                                            {:gene :apply}
;;                                            {:gene :var :name `lib/>'}
;;                                            {:gene :apply}]
;;                                     :locals    ['in1]
;;                                     :ret-type  {:type 'boolean?} ;;[!!]
;;                                     :type-env  (assoc lib/type-env
;;                                                       'in1 {:type :map-of :key {:type 'int?} :value {:type 'int?}})
;;                                     :dealiases lib/dealiases})))))

(deftest psb-vectors-summed
  (testing "map2v Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 0}
                                                    {:gene :local :idx 1}
                                                    {:gene :var :name '+}
                                                    {:gene :var :name `lib/map2v}
                                                    {:gene :apply}]
                                        :locals    ['in1 'in2]
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  (assoc lib/type-env 
                                                          'in1 {:type :vector :child {:type 'int?}}
                                                          'in2 {:type :vector :child {:type 'int?}})
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1 ~'in2] ~form))]
      (is (= [2 -2] (func [1 -1] [1 -1]))))))
