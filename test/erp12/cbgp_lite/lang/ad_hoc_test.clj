(ns erp12.cbgp-lite.lang.ad-hoc-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.cbgp-lite.lang.ast :as a]
            [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.lang.lib :as lib]
            [mb.hawk.core]
            [taoensso.timbre :as log]))

;; Uncomment to ignore debugging for testing
(log/set-min-level! :report)

(def verbose false)

(deftest split-str-test
  (testing "split-str"
    (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :local :idx 1}
                                                                {:gene :local :idx 0}
                                                                {:gene :var :name `lib/split-str}
                                                                {:gene :apply}]
                                                    :locals    ['in1 'in2]
                                                    :ret-type  {:type :vector :child {:type 'string?}}
                                                    :type-env  (assoc lib/type-env
                                                                      'in1 {:type 'string?}
                                                                      'in2 {:type 'string?})
                                                    :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'string?}} type))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1 ~'in2] ~form))]
      (is (= ["good " "orning!"] (func "good morning!" "m")))))
  
  (testing "split-str-on-char"
    (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :local :idx 1}
                                                                {:gene :local :idx 0}
                                                                {:gene :var :name `lib/split-str}
                                                                {:gene :apply}]
                                                    :locals    ['in1 'in2]
                                                    :ret-type  {:type :vector :child {:type 'string?}}
                                                    :type-env  (assoc lib/type-env
                                                                      'in1 {:type 'string?}
                                                                      'in2 {:type :s-var :sym 't :typeclasses #{:stringable}})
                                                    :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'string?}} type))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1 ~'in2] ~form))]
      (is (= ["good " "orning!"] (func "good morning!" \m)))))
  
    (testing "split-str-on-ws"
    (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :local :idx 0}
                                                                {:gene :var :name `lib/split-str-on-ws}
                                                                {:gene :apply}]
                                                    :locals    ['in1]
                                                    :ret-type  {:type :vector :child {:type 'string?}}
                                                    :type-env  (assoc lib/type-env
                                                                      'in1 {:type 'string?})
                                                    :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'string?}} type))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1] ~form))]
      (is (= ["good" "morning!"] (func "good morning!")))))
  )

(deftest count-test
  (testing "count"
    (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :local :idx 0}
                                                                {:gene :var :name 'count}
                                                                {:gene :apply}]
                                                    :locals    ['in1]
                                                    :ret-type  {:type 'int?}
                                                    :type-env  (assoc lib/type-env
                                                                      'in1 {:type :s-var :sym 'c :typeclasses #{:countable}})
                                                    :dealiases lib/dealiases}))
          _ (is (= type {:type 'int?}))
          form (a/ast->form ast)
          func (eval `(fn [~'in1] ~form))]
      (is (= (func ["a" "b" "c"]) 3))
      (is (= (func [1 3 4 5 10]) 5))
      (is (= (func []) 0))
      (is (= (func #{1 2 3}) 3))
      (is (= (func {1 "hi" 2 "world"}) 2))
      (is (= (func "testing!") 8))
      #_(is (= (func 4) 1)))))

(deftest empty-test
  (testing "empty"
    (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :local :idx 0}
                                                                {:gene :var :name 'empty?}
                                                                {:gene :apply}]
                                                    :locals    ['in1]
                                                    :ret-type  {:type 'boolean?}
                                                    :type-env  (assoc lib/type-env
                                                                      'in1 {:type :s-var :sym 'c :typeclasses #{:countable}})
                                                    :dealiases lib/dealiases}))
          _ (is (= type {:type 'boolean?}))
          form (a/ast->form ast)
          func (eval `(fn [~'in1] ~form))]
      (is (= (func ["a" "b" "c"]) false))
      (is (= (func [1 3 4 5 10]) false))
      (is (= (func []) true))
      (is (= (func #{1 2 3}) false))
      (is (= (func {1 "hi" 2 "world"}) false))
      (is (= (func "testing!") false))
      (is (= (func "") true))
      (is (= (func #{}) true)))))

(deftest index-of-test
  (testing "index of"
    (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :local :idx 1}
                                                                {:gene :local :idx 0}
                                                                {:gene :var :name `lib/index-of}
                                                                {:gene :apply}]
                                                    :locals    ['in1 'in2]
                                                    :ret-type  {:type 'int?}
                                                    :type-env  (assoc lib/type-env
                                                                      'in1 {:type :s-var :sym 'c :typeclasses #{:indexable}}
                                                                      'in2 {:type :s-var :sym 'a})
                                                    :dealiases lib/dealiases}))
          _ (is (= {:type 'int?} type))
          form (a/ast->form ast)
          func (eval `(fn [~'in1 ~'in2] ~form))]
      (is (= (func ["a" "b" "c"] "c") 2))
      (is (= (func [[0 1] [1 2] [2 3]] [0 1]) 0))
      (is (= (func [1 3 4 5 10] 5) 3))
      (is (= (func [1 3 4 5 10] 20) -1))
      (is (= (func [] 1) -1))
      (is (= (func "hello!" \!) 5))
      (is (= (func "hello!" "lo") 3)))))

;;indexable (:vector 'string?)
(deftest first-test
  (testing "Vector First"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                    {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name 'first}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'int?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'int?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= 55 (func)))))

  (testing "String First"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val \j :type {:type 'char?}}
                                                    {:gene :lit :val "Hello" :type {:type 'string?}}
                                                    {:gene :var :name 'first}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'char?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'char?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= \H (func)))))

  (testing "Set First (not applied)"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                    {:gene :lit :val #{1 2 3 4} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :var :name 'first}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'int?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'int?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= 42 (func)))))

  (testing "Map First (not applied)"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                    {:gene :lit :val {1 2 3 4} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                    {:gene :var :name 'first}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'int?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'int?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= 42 (func))))))

(deftest last-test
  (testing "Last Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                    {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name 'last}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'int?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'int?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= 77 (func)))))

  (testing "Last String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val \j :type {:type 'char?}}
                                                    {:gene :lit :val "Hello" :type {:type 'string?}}
                                                    {:gene :var :name 'last}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'char?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'char?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= \o (func))))))

(deftest rest-test
  (testing "Rest Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [89 98] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name `lib/rest'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type :vector :child {:type 'int?}}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [66 77] (func)))))

  (testing "Rest String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val "hi" :type {:type 'string?}}
                                                    {:gene :lit :val "Hello" :type {:type 'string?}}
                                                    {:gene :var :name `lib/rest'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'string?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= "ello" (func)))))
  (testing "Rest Set (not applied)"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                    {:gene :lit :val #{1 2 3 4} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :var :name 'rest}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'int?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'int?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= 42 (func))))))

(deftest butlast-test
  (testing "Butlast Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [89 98] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name `lib/butlast'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type :vector :child {:type 'int?}}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [55 66] (func)))))

  (testing "Butlast String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val "hi" :type {:type 'string?}}
                                                    {:gene :lit :val "Hello" :type {:type 'string?}}
                                                    {:gene :var :name `lib/butlast'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'string?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= "Hell" (func)))))
  (testing "Butlast Set (not applied)"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                    {:gene :lit :val #{1 2 3 4} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :var :name `lib/butlast'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'int?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'int?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= 42 (func))))))

(deftest nth-test
  (testing "Nth Test Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 99 :type {:type 'int?}}
                                                    {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val 2 :type {:type 'int?}}
                                                    {:gene :var :name `lib/safe-nth}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'int?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'int?} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= 77 (func)))))

  (testing "Nth Test String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val \k :type {:type 'char?}}
                                                    {:gene :lit :val "Hello there" :type {:type 'string?}}
                                                    {:gene :lit :val 4 :type {:type 'int?}}
                                                    {:gene :var :name `lib/safe-nth}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'char?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'char?} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= \o (func))))))

(deftest take-test
  (testing "Take String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val "Hello there" :type {:type 'string?}}
                                                    {:gene :lit :val 5 :type {:type 'int?}}
                                                    {:gene :var :name `lib/take'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= "Hello" (func)))))
  (testing "Take Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [1 2 3 4 5 6 7 8 9] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val 5 :type {:type 'int?}}
                                                    {:gene :var :name `lib/take'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'int?}} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [1 2 3 4 5] (func)))))
  (testing "Take Set (not applied)"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val #{1 2 3 4 5 6 7 8 9} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :lit :val 5 :type {:type 'int?}}
                                                    {:gene :var :name `lib/take'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'int?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'int?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= 5 (func))))))

(deftest reverse-test
  (testing "Reverse Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [1 2 3 4 5] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name `lib/reverse'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'int?}} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [5 4 3 2 1] (func)))))
  (testing "Reverse String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val "notlimaH" :type {:type 'string?}}
                                                    {:gene :var :name `lib/reverse'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= "Hamilton" (func)))))
  (testing "Reverse Set (not applied)"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val #{1 2 3 4 5} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :var :name `lib/reverse'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :set :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :set :child {:type 'int?}} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= #{1 2 3 4 5} (func))))))

(deftest safe-sub-test
  (testing "Safe Substring"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 3 :type {:type 'int?}}
                                                    {:gene :lit :val 0 :type {:type 'int?}}
                                                    {:gene :lit :val "Hamilton" :type {:type 'string?}}
                                                    {:gene :var :name `lib/safe-sub-coll}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= "Ham" (func))))))

(deftest remove-element-test
  (testing "Vector Remove Element"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [6] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [55 66 0 77 0] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val 77 :type {:type 'int?}}
                                                    {:gene :var :name `lib/remove-element}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [55 66 0 0] (func)))))

  (testing "String Remove Element Test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val "hello there" :type {:type 'string?}}
                                                    {:gene :lit :val "hello world!!" :type {:type 'string?}}
                                                    {:gene :lit :val \w  :type {:type 'char?}}
                                                    {:gene :var :name `lib/remove-element}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= 'string? (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= "hello orld!!" (func))))))

;; Collection Conversion
;; Vec, Set, ->map
(deftest coll-cast-test
  (testing "Vector Cast"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                    {:gene :local :idx 0}
                                                    {:gene :var :name 'vec}
                                                    {:gene :apply}]
                                        :locals    ['in1]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type :s-var :sym 'c :typeclasses #{:countable}})
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1] ~form))]
      (is (= [[1 "hello"] [3 "world"]] (func {1 "hello" 3 "world"})))
      (is (= [1 4 9 5 10] (func #{1 4 9 5 10})))
      (is (= [\h \e \l \l \o \!] (func "hello!")))
      (is (= [] (func [])))
      (is (= [1 2 3] (func [1 2 3])))
      (is (thrown?
           java.lang.RuntimeException
           (func 5)))))
  (testing "Set Cast"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                    {:gene :local :idx 0}
                                                    {:gene :var :name 'set}
                                                    {:gene :apply}]
                                        :locals    ['in1]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type :s-var :sym 'c :typeclasses #{:countable}})
                                        :dealiases lib/dealiases}))
          _ (is (= :set (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1] ~form))]
      (is (= #{[1 "hello"] [3 "world"]} (func {1 "hello" 3 "world"})))
      (is (= #{\a \b \c \d} (func [\a \b \c \d])))
      (is (= #{} (func [])))
      (is (= #{\t \e \s} (func "test"))))) ; should this error b/c no overload? 

  (testing "Map Cast"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                    {:gene :local :idx 0}
                                                    {:gene :var :name `lib/->map}
                                                    {:gene :apply}]
                                        :locals    ['in1]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type :s-var :sym 'c :typeclasses #{:countable}})
                                        :dealiases lib/dealiases}))
          _ (is (= :map-of (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1] ~form))]
      (is (= {1 "hello" 3 "world"} (func #{[1 "hello"] [3 "world"]})))
      (is (= {\a 26 \m 14 \n 13 \z 1} (func [[\a 26] [\m 14] [\n 13] [\z 1]])))
      (is (= {} (func [])))
      (is (thrown?
           java.lang.IllegalArgumentException
           (func "test"))))))

;; Combining collections
(deftest conj-test
  (testing "Conj Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val [1 2 3 4] :type {:type :vector :child {:type 'int?}}}
                                               {:gene :lit :val 5 :type {:type 'int?}}
                                               {:gene :var :name `lib/conj'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type :vector :child {:type 'int?}}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'int?}} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= [1 2 3 4 5] (func)))))

  (testing "Conj Sets"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val #{\h \g \e \t} :type {:type :set :child {:type 'char?}}}
                                               {:gene :lit :val \n :type {:type 'char?}}
                                               {:gene :var :name `lib/conj'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type :set :child {:type 'char?}}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :set :child {:type 'char?}} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= #{\h \g \e \t \n} (func)))))

  (testing "Conj Strings (not applied)"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val "hello world" :type {:type 'string?}}
                                               {:gene :lit :val 5 :type {:type 'int?}}
                                               {:gene :var :name `lib/conj'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type {:type 'string?}}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= "hello world" (func))))))

(deftest concat-test
  (testing "Concat Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val [4.4 5.5] :type {:type :vector :child {:type 'double?}}}
                                               {:gene :lit :val [1.1 2.2 3.3] :type {:type :vector :child {:type 'double?}}}
                                               {:gene :var :name `lib/concat'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type :vector :child {:type 'double?}}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'double?}} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= [1.1 2.2 3.3 4.4 5.5] (func)))))
  (testing "Conact String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val "College" :type {:type 'string?}}
                                               {:gene :lit :val "Hamilton " :type {:type 'string?}}
                                               {:gene :var :name `lib/concat'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'string?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= "Hamilton College" (func)))))
  (testing "Set (not applied)"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val #{1 2 3} :type {:type :set :child {:type 'int?}}}
                                               {:gene :lit :val  #{4 5 6} :type {:type :set :child {:type 'int?}}}
                                               {:gene :var :name `lib/concat'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type :set :child {:type 'int?}}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :set :child {:type 'int?}} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= #{4 5 6} (func))))))
;;join to-do [!!]

;; Modifying collections
(deftest replace-test
  (testing "Replace Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val 101 :type {:type 'int?}}
                                               {:gene :lit :val 3 :type {:type 'int?}}
                                               {:gene :lit :val [1 2 3 3 4 2 3] :type {:type :vector :child {:type 'int?}}}
                                               {:gene :var :name `lib/replace'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type :vector :child {:type 'int?}}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'int?}} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= [1 2 101 101 4 2 101] (func)))))
  (testing "Replace String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val \a :type {:type 'char?}}
                                               {:gene :lit :val \e :type {:type 'char?}}
                                               {:gene :lit :val "Here" :type {:type 'string?}}
                                               {:gene :var :name `lib/replace'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'string?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= "Hara" (func))))
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val "troll" :type {:type 'string?}}
                                               {:gene :lit :val "ut" :type {:type 'string?}}
                                               {:gene :lit :val "Computer" :type {:type 'string?}}
                                               {:gene :var :name `lib/replace'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'string?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= "Comptroller" (func))))))

(deftest replace-first-test
  (testing "Replace First Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val 64 :type {:type 'int?}}
                                               {:gene :lit :val 3 :type {:type 'int?}}
                                               {:gene :lit :val [1 2 3 3 3 4 5] :type {:type :vector :child {:type 'int?}}}
                                               {:gene :var :name `lib/replace-first'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type :vector :child {:type 'int?}}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'int?}} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= [1 2 64 3 3 4 5] (func)))))
  (testing "Replace First String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val \a :type {:type 'char?}}
                                               {:gene :lit :val \e :type {:type 'char?}}
                                               {:gene :lit :val "Here" :type {:type 'string?}}
                                               {:gene :var :name `lib/replace-first'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'string?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= "Hare" (func))))))

;; Higher Order functions
(deftest reduce-test
  (testing "Reduce Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val 0 :type {:type 'int?}}
                                               {:gene :lit :val [1 2 3 4 5] :type {:type :vector :child {:type 'int?}}}
                                               {:gene :var :name '+}
                                               {:gene :var :name 'reduce}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'int?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'int? :typeclasses #{:number}} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= 15 (func)))))
  (testing "Reduce Set"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val 0 :type {:type 'int?}}
                                               {:gene :lit :val #{1 2 3 4 5} :type {:type :set :child {:type 'int?}}}
                                               {:gene :var :name '+}
                                               {:gene :var :name 'reduce}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'int?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'int? :typeclasses #{:number}} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= 15 (func)))))

  ;; REDUCE WITH MAP does not work
  #_(testing "Reduce Map"
      (let [{::c/keys [ast type]} (:ast (c/push->ast
                                         {:push [{:gene :lit :val 0 :type {:type 'int?}}
                                                 {:gene :lit :val {\a 1 \b 2 \c 3 \d 4} :type {:type :map-of :key {:type 'char?} :value {:type 'int?}}}
                                                 {:gene :fn :arg-types [(lib/tuple-of lib/CHAR lib/INT)] :ret-type (lib/tuple-of lib/CHAR lib/INT)}
                                                 [{:gene :lit :val 0 :type {:type 'int?}}
                                                  {:gene :local :idx 0}
                                                  {:gene :var :name 'first}
                                                  {:gene :apply}
                                                  {:gene :var :name 'int}
                                                  {:gene :apply}
                                                  {}]
                                                 {:gene :var :name 'reduce}
                                                 {:gene :apply}]
                                          :locals []
                                          :ret-type {:type 'int?}
                                          :type-env lib/type-env
                                          :dealiases lib/dealiases}))
            _ (is (= {:type 'int? :typeclasses #{:number}} type))
            _ (when verbose (println "REAL-AST: " ast))
            form (a/ast->form ast)
            _ (when verbose (println "FORM: " form))
            func (eval `(fn [] ~form))
            _ (when verbose (println "FUNC:" func))]
        (is (= 15 (func))))))

(deftest fold-test
  (testing "Fold Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val 0 :type {:type 'int?}}
                                               {:gene :lit :val [1 2 3 4 5] :type {:type :vector :child {:type 'int?}}}
                                               {:gene :var :name '+}
                                               {:gene :var :name 'fold}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'int?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'int? :typeclasses #{:number}} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))
          _ (when verbose (println "FUNC:" func))]
      (is (= 15 (func)))))

  #_(testing "Fold Map"
      (let [{::c/keys [ast type]} (:ast (c/push->ast
                                         {:push [{:gene :lit :val 0 :type {:type 'int?}}
                                                 {:gene :lit :val {1 2 3 4 5 6} :type {:type :map :child {:type 'int?}}}
                                                 {:gene :var :name '+}
                                                 {:gene :var :name 'fold}
                                                 {:gene :apply}]
                                          :locals []
                                          :ret-type {:type 'int?}
                                          :type-env lib/type-env
                                          :dealiases lib/dealiases}))
            _ (is (= {:type 'int? :typeclasses #{:number}} type))
            _ (when verbose (println "REAL-AST: " ast))
            form (a/ast->form ast)
            _ (when verbose (println "FORM: " form))
            func (eval `(fn [] ~form))
            _ (when verbose (println "FUNC:" func))]
        (is (= 15 (func))))))

(deftest remove-test
  (testing "Remove Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [6] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [55 66 0 77 0] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name 'zero?}
                                                    {:gene :var :name `lib/remove'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [55 66 77] (func)))))

  (testing "Remove Set"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val #{6} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :lit :val #{55 66 0 77 8} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :var :name 'zero?}
                                                    {:gene :var :name `lib/remove'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :set :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :set (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= #{55 66 77 8} (func)))))

  (testing "Map Remove"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val {1 4, 98 3} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                    {:gene :lit :val {40 \h, 50 \e, 200 \p} :type {:type :map-of :key {:type 'int?} :value {:type 'char?}}}
                                                    {:gene :fn :arg-types [(lib/tuple-of lib/INT lib/CHAR)] :ret-type lib/BOOLEAN}
                                                    [{:gene :local :idx 0}
                                                     {:gene :var :name 'left}
                                                     {:gene :apply}
                                                     {:gene :local :idx 0}
                                                     {:gene :var :name 'right}
                                                     {:gene :apply}
                                                     {:gene :var :name 'int}
                                                     {:gene :apply}
                                                     {:gene :var :name `lib/>'}
                                                     {:gene :apply}]
                                                    {:gene :var :name `lib/remove'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :map-of :key {:type 'int?} :value {:type 'char?}} ;;[!!]
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :map-of (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= {200 \p} (func)))))

  (testing "Remove String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val \k :type {:type 'char?}}
                                                    {:gene :lit :val "hi there isabelle" :type {:type 'string?}}
                                                    {:gene :fn :arg-types [lib/CHAR] :ret-type lib/BOOLEAN}
                                                    [{:gene :local :idx 0}
                                                     {:gene :lit :val \i :type {:type 'char?}}
                                                     {:gene :var :name '=}
                                                     {:gene :apply}]
                                                    {:gene :var :name `lib/remove'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'string?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= "h there sabelle" (func))))))

(deftest filter-test
  (testing "Filter Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [6] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [55 66 0 77 0] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name 'zero?}
                                                    {:gene :var :name `lib/filter'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [0 0] (func)))))
  (testing "Filter Set"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val #{6} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :lit :val #{55 66 0 77 8} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :var :name 'zero?}
                                                    {:gene :var :name `lib/filter'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :set :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :set (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= #{0} (func)))))

  (testing "Map Filter"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val {1 4, 98 3} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                    {:gene :lit :val {40 \h, 50 \e, 200 \p} :type {:type :map-of :key {:type 'int?} :value {:type 'char?}}}
                                                    {:gene :fn :arg-types [(lib/tuple-of lib/INT lib/CHAR)] :ret-type lib/BOOLEAN}
                                                    [{:gene :local :idx 0}
                                                     {:gene :var :name 'left}
                                                     {:gene :apply}
                                                     {:gene :local :idx 0}
                                                     {:gene :var :name 'right}
                                                     {:gene :apply}
                                                     {:gene :var :name 'int}
                                                     {:gene :apply}
                                                     {:gene :var :name `lib/>'}
                                                     {:gene :apply}]
                                                    {:gene :var :name `lib/filter'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :map-of :key {:type 'int?} :value {:type 'char?}} ;;[!!]
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :map-of (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= {40 \h, 50 \e} (func)))))

  (testing "Filter String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val \k :type {:type 'char?}}
                                                    {:gene :lit :val "hi there isabelle" :type {:type 'string?}}
                                                    {:gene :fn :arg-types [lib/CHAR] :ret-type lib/BOOLEAN}
                                                    [{:gene :local :idx 0}
                                                     {:gene :lit :val \i :type {:type 'char?}}
                                                     {:gene :var :name '=}
                                                     {:gene :apply}]
                                                    {:gene :var :name `lib/filter'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'string?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= "ii" (func))))))
;; [!!]
(deftest mapcat-vector-test
  (testing "Mapcat Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [6 5 9] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name `lib/mapcat'}
                                                    {:gene :var :name `lib/reverse'}
                                                    {:gene :lit :val [[1 2 3] [55 6 98]] :type {:type :vector :child {:type :vector :child {:type 'int?}}}}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [3 2 1 98 6 55] (func))))))

(deftest sort-test
  (testing "Sort Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [6] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [9 4 1 4 3 15] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name `lib/sort'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [1 3 4 4 9 15] (func)))))
  (testing "Sort String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val "fledcbxa" :type {:type 'string?}}
                                                    {:gene :var :name `lib/sort'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= "abcdeflx" (func))))))

(deftest contains?-test
  (testing "contains? Map"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 1 :type {:type 'int?}}
                                                    {:gene :lit :val {1 2 3 4} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                    {:gene :var :name 'contains?}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'boolean?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'boolean?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= true (func)))))

  (testing "contains? Set"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 4 :type {:type 'int?}}
                                                    {:gene :lit :val #{1 2 3 4} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :var :name 'contains?}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'boolean?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type 'boolean?}))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= true (func)))))

  (testing "contains? Invalid types"
    (testing "contains? Set"
      (let [{::c/keys [ast type]} (:ast (c/push->ast
                                         {:push      [{:gene :lit :val false :type {:type 'boolean?}}
                                                      {:gene :lit :val 5 :type {:type 'int?}}
                                                      {:gene :lit :val [1 2 3 4] :type {:type :vector :child {:type 'int?}}}
                                                      {:gene :var :name 'contains?}
                                                      {:gene :apply}]
                                          :locals    []
                                          :ret-type  {:type 'boolean?}
                                          :type-env  lib/type-env
                                          :dealiases lib/dealiases}))
            _ (is (= type {:type 'boolean?}))
            _ (when verbose (println "REAL-AST: " ast))
            form (a/ast->form ast)
            _ (when verbose (println "FORM: " form))
            func (eval `(fn [] ~form))]
        (is (= false (func)))))))

(deftest mapv-test
  (testing "mapv Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :var :name 'inc}
                                                    {:gene :lit :val [1 2 3] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name 'mapv}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [2 3 4] (func)))))

  (testing "mapv Set"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val #{5 6 7 8} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :var :name 'inc}
                                                    {:gene :var :name 'mapv}
                                                    {:gene :apply}
                                                    {:gene :var :name `lib/sort'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [6 7 8 9] (func)))))

  (testing "mapv string"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :var :name 'int}
                                                    {:gene :lit :val "hi there" :type {:type 'string?}}
                                                    {:gene :var :name 'mapv}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [104 105 32 116 104 101 114 101] (func)))))

  ; form: (mapv #(assoc [\a 30] 2 %) (keys {\a 1 \b 10}))
  (mapv #(assoc [\a 30] 2 %) (keys {\a 1 \b 10}))
  (mapv #(assoc [\a 30] 2 (first %)) {\a 1 \b 10})

  (testing "mapv map"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val {1 4, 98 3} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                    {:gene :lit :val {40 \h, 50 \e, 200 \p} :type {:type :map-of :key {:type 'int?} :value {:type 'char?}}}
                                                    {:gene :fn :arg-types [(lib/tuple-of lib/INT lib/CHAR)] :ret-type lib/BOOLEAN}
                                                    [{:gene :local :idx 0}
                                                     {:gene :var :name 'left}
                                                     {:gene :apply}
                                                     {:gene :local :idx 0}
                                                     {:gene :var :name 'right}
                                                     {:gene :apply}
                                                     {:gene :var :name 'int}
                                                     {:gene :apply}
                                                     {:gene :var :name `lib/>'}
                                                     {:gene :apply}]
                                                    {:gene :var :name 'mapv}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'boolean?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (println)
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [true true false] (func))))))

(deftest map2v-test
  (testing "map2v Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [1 2 3] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [4 5 6] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name '+}
                                                    {:gene :var :name `lib/map2v}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [5 7 9] (func)))))

  (testing "map2v string"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val "hello hi" :type {:type 'string?}}
                                                    {:gene :lit :val "hi there" :type {:type 'string?}}
                                                    {:gene :fn :arg-types [lib/CHAR lib/CHAR] :ret-type lib/INT}
                                                    [{:gene :local :idx 0}
                                                     {:gene :var :name 'int}
                                                     {:gene :apply}
                                                     {:gene :local :idx 1}
                                                     {:gene :var :name 'int}
                                                     {:gene :apply}
                                                     {:gene :var :name '+}
                                                     {:gene :apply}]
                                                    {:gene :var :name `lib/map2v}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :vector (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [] ~form))]
      (is (= [208 206 140 224 215 133 218 206] (func))))))

;; Functional Programming
;; comp (comp2-fn1, comp3-fn1, comp2-fn2, comp3-fn2), partial
(deftest comp-test
  (testing "comp2-fn1-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 0}
                                                    {:gene :var :name 'inc}
                                                    {:gene :var :name `lib/square}
                                                    {:gene :var :name 'comp}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= 'int? (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1] ~form))]
      (is (= 25 (func 4)))))
  
  (testing "comp3-fn1-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 0}
                                                    {:gene :var :name `lib/square}
                                                    {:gene :var :name 'inc}
                                                    {:gene :var :name 'inc}
                                                    {:gene :var :name 'comp}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= 'int? (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1] ~form))]
      (is (= 18 (func 4)))))
  
  (testing "comp2-fn2-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 1}
                                                    {:gene :local :idx 0}
                                                    {:gene :var :name '+}
                                                    {:gene :var :name 'dec}
                                                    {:gene :var :name 'comp}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1 'in2 'in3]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?}
                                                          'in2 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= 'int? (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1 ~'in2] ~form))]
      (is (= 14 (func 10 5)))))

  (testing "comp3-fn2-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 2}
                                                    {:gene :local :idx 1}
                                                    {:gene :local :idx 0}
                                                    {:gene :var :name '+}
                                                    {:gene :var :name 'dec}
                                                    {:gene :var :name 'str}
                                                    {:gene :var :name 'comp}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1 'in2 'in3]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?}
                                                          'in2 {:type 'int?}
                                                          'in3 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= 'string? (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1 ~'in2 ~'in3] ~form))]
      (is (= "14" (func 10 5 1))))))

(deftest partial-test
  ; partial (partial1-fn2, partial1-fn3, partial2-fn3)
  ; target form: ((partial * 100) 5)
  (testing "partial1-fn2-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 0}
                                                    {:gene :lit :val 100 :type {:type 'int?}}
                                                    {:gene :var :name '*}
                                                    {:gene :var :name 'partial}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1]
                                        :ret-type  {:type 'int?}
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= 'int? (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1] ~form))]
      (is (= 500 (func 5)))))

    ; target form: ((partial assoc {\a 1 \b 2 \c 3}) \z 42)
  (testing "partial1-fn3-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 0}
                                                    {:gene :lit :val {\a 1 \b 2 \c 3} :type {:type :map-of :key {:type 'char?} :value {:type 'int?}}}
                                                    {:gene :var :name 'assoc}
                                                    {:gene :var :name 'partial}
                                                    {:gene :apply}
                                                    {:gene :lit :val \z :type {:type 'char?}}
                                                    {:gene :apply}]
                                        :locals    ['in1]
                                        :ret-type  {:type :map-of :key {:type 'char?} :value {:type 'int?}}
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= :map-of (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1] ~form))]
      (is (= '((partial assoc {\a 1, \b 2, \c 3}) \z in1) form))
      (is (= {\a 1 \b 2 \c 3 \z 42} (func 42)))))

 ; target form: ((partial assoc {\a 1 \b 2 \c 3} \z) 42)
  (testing "partial2-fn3-test-A"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 0}
                                                    {:gene :lit :val \z :type {:type 'char?}}
                                                    {:gene :lit :val {\a 1 \b 2 \c 3} :type {:type :map-of :key {:type 'char?} :value {:type 'int?}}}
                                                    {:gene :var :name 'assoc}
                                                    {:gene :var :name 'partial}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1]
                                        :ret-type  {:type :map-of :key {:type 'char?} :value {:type 'int?}}
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= :map-of (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1] ~form))]
      (is (= '((partial assoc {\a 1, \b 2, \c 3} \z) in1) form))
      (is (= {\a 1 \b 2 \c 3 \z 42} (func 42)))))
  
  ; target form: ((partial test3 param1 param2) param3)
  ;          eg. ((partial assoc {:a 1 :b 2 :c 3} :z) 42)
  (testing "partial2-fn3-test-B"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 1}
                                                    {:gene :local :idx 0}
                                                    {:gene :lit :val {0 42 1 2999 2 108} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                    {:gene :var :name 'assoc}
                                                    {:gene :var :name 'partial}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1 'in2]
                                        :ret-type  {:type :map-of :key {:type 'int?} :value {:type 'int?}}
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?}
                                                          'in2 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= :map-of (:type type)))
          _ (when verbose (println "REAL-AST: " ast))
          form (a/ast->form ast)
          _ (when verbose (println "FORM: " form))
          func (eval `(fn [~'in1 ~'in2] ~form))]
      (is (= {0 42 1 2999 2 108 3 42} (func 3 42))))))
