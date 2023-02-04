(ns erp12.cbgp-lite.lang.compile-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.walk :as w]
            [erp12.cbgp-lite.lang.ast :as a]
            [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.schema :as schema]
            [hawk.core]
            [meander.epsilon :as m])
  (:import (java.io StringWriter)))

(defmacro matches?
  [template value]
  (let [template (w/postwalk (fn [node]
                               (if (and (symbol? node)
                                        (str/starts-with? (name node) "?"))
                                 `(m/pred some? ~node)
                                 node))
                             template)]
    `(m/match ~value
              ~template true
              ~(quote ?else) false)))

(deftest matches?-test
  (is (matches? {:op ?_ :name ?a}
                {:op :var :name 'foo}))
  (is (not (matches? {:op ?_ :name ?a}
                     {:op :var :var 'foo})))
  (is (not (matches? {:op :VAR :name ?a}
                     {:op :var :name 'foo})))
  (is (matches? {:a ?x :b ?x}
                {:a 1 :b 1 :c "extra"})))

(deftest nth-local-test
  (is (= (c/nth-local 100 {:locals ['x 'y 'z]}) 'y))
  (is (nil? (c/nth-local 100 {:locals []}))))

(deftest macro?-test
  (is (c/macro? {:op :var :var 'if}))
  (is (not (c/macro? {:op :var :var 'double-add}))))

(deftest pop-ast-test
  (is (= (c/pop-ast c/empty-state)
         {:ast :none :state c/empty-state}))
  (is (= (c/pop-ast {:asts [{::c/ast  :_
                             ::c/type {:type 'int?}}]})
         {:ast   {::c/ast :_ ::c/type {:type 'int?}}
          :state {:asts (list)}}))
  (testing "popping functions of any type"
    (is (= (c/pop-function-ast {:asts (list {::c/ast  :_
                                             ::c/type {:type 'boolean?}}
                                            {::c/ast  :_
                                             ::c/type {:type   :=>
                                                       :input  {:type     :cat
                                                                :children [{:type 'int?}]}
                                                       :output {:type 'string?}}})})
           {:ast   {::c/ast  :_
                    ::c/type {:type   :=>
                              :input  {:type     :cat
                                       :children [{:type 'int?}]}
                              :output {:type 'string?}}}
            :state {:asts (list {::c/ast  :_
                                 ::c/type {:type 'boolean?}})}}))

    (is (= (c/pop-function-ast {:asts (list {::c/ast  :_
                                             ::c/type {:type 'boolean?}}
                                            {::c/ast  :_
                                             ::c/type {:type   :scheme
                                                       :s-vars ['a]
                                                       :body   {:type   :=>
                                                                :input  {:type     :cat
                                                                         :children [{:type :s-var :sym 'a}]}
                                                                :output {:type :s-var :sym 'a}}}})})
           {:ast   {::c/ast  :_
                    ::c/type {:type   :scheme
                              :s-vars ['a]
                              :body   {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type :s-var :sym 'a}]}
                                       :output {:type :s-var :sym 'a}}}}
            :state {:asts (list {::c/ast  :_
                                 ::c/type {:type 'boolean?}})}})))

  (testing "popping function of specific type"
    (testing "monomorphic function"
      (is (= (c/pop-unifiable-ast {:type   :=>
                                   :input  {:type     :cat
                                            :children [{:type 'int?}]}
                                   :output {:type 'string?}}
                                  {:asts (list {::c/ast  :_
                                                ::c/type {:type 'boolean?}}
                                               {::c/ast  :_
                                                ::c/type {:type   :=>
                                                          :input  {:type     :cat
                                                                   :children [{:type 'int?}]}
                                                          :output {:type 'string?}}})})
             {:ast      {::c/ast  :_
                         ::c/type {:type   :=>
                                   :input  {:type     :cat
                                            :children [{:type 'int?}]}
                                   :output {:type 'string?}}}
              :state    {:asts (list {::c/ast  :_
                                      ::c/type {:type 'boolean?}})}
              :bindings {}})))
    (testing "failed polymorphic function"
      (let [init-state {:asts (list {::c/ast  :_
                                     ::c/type {:type 'boolean?}}
                                    {::c/ast  :_
                                     ::c/type {:type   :=>
                                               :input  {:type     :cat
                                                        :children [{:type :s-var :sym 'a}]}
                                               :output {:type :s-var :sym 'a}}})}]
        (is (= (c/pop-unifiable-ast {:type   :=>
                                     :input  {:type     :cat
                                              :children [{:type 'int?}]}
                                     :output {:type 'string?}}
                                    init-state)
               {:ast      :none
                :state    init-state
                :bindings {}}))))
    (testing "successful polymorphic function"
      (is (= (c/pop-unifiable-ast {:type   :=>
                                   :input  {:type     :cat
                                            :children [{:type 'int?}]}
                                   :output {:type 'string?}}
                                  {:asts (list {::c/ast  :_
                                                ::c/type {:type 'boolean?}}
                                               {::c/ast  :_
                                                ::c/type {:type   :=>
                                                          :input  {:type     :cat
                                                                   :children [{:type :s-var :sym 'a}]}
                                                          :output {:type :s-var :sym 'b}}})})
             {:ast      {::c/ast  :_
                         ::c/type {:type   :=>
                                   :input  {:type     :cat
                                            :children [{:type :s-var :sym 'a}]}
                                   :output {:type :s-var :sym 'b}}}
              :state    {:asts (list {::c/ast  :_
                                      ::c/type {:type 'boolean?}})}
              :bindings {'a {:type 'int?}
                         'b {:type 'string?}}}))))

  (testing "skip macros"
    (is (= (c/pop-ast {:asts (list {::c/ast {:op :var :var 'if} :type :_}
                                   {::c/ast {:op :var :var '+} :type :_})})
           {:ast   {::c/ast {:op :var :var '+} :type :_}
            :state {:asts (list {::c/ast {:op :var :var 'if} :type :_})}}))))

(deftest compile-step-test
  (testing "compile lit"
    (is (partial= {:asts   (list {::c/ast  {:op :const :val 1}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :lit :val 1 :type {:type 'int?}}
                                   :state     c/empty-state
                                   :type-env  {}}))))
  (testing "compile var"
    (is (matches? {:asts   ({::c/ast  {:op :var :var '=}
                             ::c/type {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type :s-var :sym ?a}
                                                           {:type :s-var :sym ?b}]}
                                       :output {:type 'boolean?}}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :var :name '=}
                                   :state     c/empty-state
                                   :type-env  lib/type-env}))))
  (testing "compile local"
    (is (= c/empty-state
           (c/compile-step {:push-unit {:gene :local :idx 3}
                            :state     c/empty-state
                            :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op :local :name 'x}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals ['x]}
                  (c/compile-step {:push-unit {:gene :local :idx 3}
                                   :state     (assoc c/empty-state :locals ['x])
                                   :type-env  {'x {:type 'int?}}}))))
  (testing "compile apply"
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var 'int-sub}
                                            :args [{:op :const :val 2}
                                                   {:op :const :val 1}]}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                :asts (list {::c/ast  {:op :const :val 2}
                                                             ::c/type {:type 'int?}}
                                                            {::c/ast  {:op :var :var 'int-sub}
                                                             ::c/type (lib/type-env 'int-sub)}
                                                            {::c/ast  {:op :const :val 1}
                                                             ::c/type {:type 'int?}}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var 'if}
                                            :args [{:op :local :name 'x}
                                                   {:op :const :val 1}
                                                   {:op :const :val -1}]}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals ['x]}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                :asts (list {::c/ast  {:op :const :val 1}
                                                             ::c/type {:type 'int?}}
                                                            {::c/ast  {:op :const :val -1}
                                                             ::c/type {:type 'int?}}
                                                            {::c/ast  {:op :var :var 'if}
                                                             ::c/type (schema/instantiate (lib/type-env 'if))}
                                                            {::c/ast  {:op :local :name 'x}
                                                             ::c/type {:type 'boolean?}})
                                                :locals ['x])
                                   :type-env  (assoc lib/type-env
                                                'x {:type 'boolean?})})))
    ;; @todo Test when args are missing
    )
  (testing "compile fn"
    (is (matches? {:asts   ({::c/ast  {:op      :fn
                                       :methods [{:op     :fn-method
                                                  :params [{:op :binding :name ?a}]
                                                  :body   {:op :local :name ?a}}]}
                             ::c/type {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type int?}]}
                                       :output {:type int?}}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene      :fn
                                               :arg-types [lib/INT]
                                               :ret-type lib/INT}
                                   :state     (assoc c/empty-state
                                                :asts (list)
                                                :push [[{:gene :local :idx 1}]])
                                   :type-env  {}})))
    (testing "nullary fn"
      (is (partial= {:asts   (list {::c/ast  {:op      :fn
                                              :methods [{:op     :fn-method
                                                         :params []
                                                         :body   {:op :var :var 'x}}]}
                                    ::c/type {:type   :=>
                                              :input  {:type :cat :children []}
                                              :output {:type 'string?}}})
                     :push   []
                     :locals []}
                    (c/compile-step {:push-unit {:gene :fn :ret-type lib/STRING}
                                     :state     (assoc c/empty-state
                                                  :asts (list {::c/ast  {:op :var :var 'x}
                                                               ::c/type {:type 'string?}})
                                                  :push [])
                                     :type-env  {'x {:type 'string?}}})))))
  (testing "compile let"
    (is (matches? {:asts   ({::c/ast  {:op       :let
                                       :bindings [{:op   :binding
                                                   :name ?v
                                                   :init {:op :const :val 1}}]
                                       :body     {:op :local :name ?v}}
                             ::c/type {:type int?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :let}
                                   :state     (assoc c/empty-state
                                                :asts (list {::c/ast  {:op :const :val 1}
                                                             ::c/type {:type 'int?}})
                                                :push [[{:gene :local :idx 1}]])
                                   :type-env  {}})))))

(deftest push->ast-test
  (testing "composing schemes"
    (is (matches? {::c/ast  {:op   :invoke
                             :fn   {:op :var :var 'identity}
                             :args [{:op :var :var 'poly-f}]}
                   ::c/type {:type   :=>
                             :input  {:type     :cat
                                      :children [{:type :s-var :sym ?x}]}
                             :output {:type :s-var :sym ?y}}}
                  (c/push->ast {:push     [{:gene :var :name 'poly-f}
                                           {:gene :var :name 'identity}
                                           {:gene :apply}]
                                :locals   []
                                :ret-type {:type   :scheme
                                           :s-vars ['a 'b]
                                           :body   {:type   :=>
                                                    :input  {:type     :cat
                                                             :children [{:type :s-var :sym 'a}]}
                                                    :output {:type :s-var :sym 'b}}}
                                :type-env {'identity {:type   :scheme
                                                      :s-vars ['c]
                                                      :body   {:type   :=>
                                                               :input  {:type     :cat
                                                                        :children [{:type :s-var :sym 'c}]}
                                                               :output {:type :s-var :sym 'c}}}
                                           'poly-f   {:type   :scheme
                                                      :s-vars ['d 'e]
                                                      :body   {:type   :=>
                                                               :input  {:type     :cat
                                                                        :children [{:type :s-var :sym 'd}]}
                                                               :output {:type :s-var :sym 'e}}}}}))))
  (testing "pruning unused function args"
    (is (= (c/push->ast {:push     [{:gene      :fn
                                     :arg-types [{:type 'int?}
                                                 {:type 'string?}]
                                     :ret-type lib/INT}
                                    [{:gene :var :name 'x}]]
                         :locals   []
                         :ret-type {:type   :=>
                                    :input  {:type :cat :children []}
                                    :output {:type 'int?}}
                         :type-env {'x {:type 'int?}}})
           {::c/ast  {:op      :fn
                      :methods [{:op     :fn-method
                                 :params []
                                 :body   {:op :var :var 'x}}]}
            ::c/type {:type   :=>
                      :input  {:type :cat :children []}
                      :output {:type 'int?}}})))
  (testing "polymorphic thunk"
    (is (matches? {::c/ast  {:op      :fn
                             :methods [{:op     :fn-method
                                        :params []
                                        :body   {:op :var :var identity}}]}
                   ::c/type {:type   :=>
                             :input  {:type :cat :children []}
                             :output {:type   :=>
                                      :input  {:type :cat :children [{:type :s-var :sym ?a}]}
                                      :output {:type :s-var :sym ?a}}}}
                  (c/push->ast {:push     [{:gene :var :name 'identity}
                                           {:gene :fn :ret-type (lib/fn-of [(lib/s-var 'a)] (lib/s-var 'a))}]
                                :locals   []
                                :ret-type {:type   :=>
                                           :input  {:type :cat :children []}
                                           :output {:type   :=>
                                                    :input  {:type :cat :children [{:type 'int?}]}
                                                    :output {:type 'int?}}}
                                :type-env {'identity {:type   :scheme
                                                      :s-vars ['a]
                                                      :body   {:type   :=>
                                                               :input  {:type :cat :children [{:type :s-var :sym 'a}]}
                                                               :output {:type :s-var :sym 'a}}}}})))))

(deftest simple-math-test
  ;; Add 100 to input.
  (let [{::c/keys [ast type]} (c/push->ast {:push      [{:gene :lit :val 100 :type {:type 'int?}}
                                                        {:gene :local :idx 0}
                                                        {:gene :var :name 'int-add}
                                                        {:gene :apply}]
                                            :locals    ['in1]
                                            :ret-type  {:type 'int?}
                                            :type-env  (assoc lib/type-env
                                                         'in1 {:type 'int?})
                                            :dealiases lib/dealiases})
        _ (is (= type {:type 'int?}))
        form (a/ast->form ast)
        _ (is (= form '(+ in1 100)))
        func (eval `(fn [~'in1] ~form))]
    (is (= 100 (func 0)))
    (is (= 101 (func 1)))))

(deftest conditional-logic-test
  ;; If input < 1000, return "small" else "large".
  (let [{::c/keys [ast type]} (c/push->ast {:push      [{:gene :lit :val "large" :type {:type 'string?}}
                                                        {:gene :lit :val "small" :type {:type 'string?}}
                                                        {:gene :lit :val 1000 :type {:type 'int?}}
                                                        {:gene :local :idx 0}
                                                        {:gene :var :name `lib/<'}
                                                        {:gene :apply}
                                                        {:gene :var :name 'if}
                                                        {:gene :apply}]
                                            :locals    ['in1]
                                            :ret-type  {:type 'string?}
                                            :type-env  (assoc lib/type-env
                                                         'in1 {:type 'int?})
                                            :dealiases lib/dealiases})
        _ (is (= type {:type 'string?}))
        form (a/ast->form ast)
        _ (is (= form '(if (erp12.cbgp-lite.lang.lib/<' in1 1000) "small" "large")))
        func (eval `(fn [~'in1] ~form))]
    (is (= (func 0) "small"))
    (is (= (func 1000) "large"))
    (is (= (func 2000) "large"))))

(deftest let-binding-test
  ;; Square and then double the input.
  (let [{::c/keys [ast type]} (c/push->ast {:push      [{:gene :local :idx 0}
                                                        {:gene :local :idx 0}
                                                        {:gene :var :name 'int-mult}
                                                        {:gene :apply}
                                                        {:gene :let}
                                                        [{:gene :local :idx 1}
                                                         {:gene :local :idx 1}
                                                         {:gene :var :name 'int-add}
                                                         {:gene :apply}]]
                                            :locals    ['in1]
                                            :ret-type  {:type 'int?}
                                            :type-env  (assoc lib/type-env
                                                         'in1 {:type 'int?})
                                            :dealiases lib/dealiases})
        _ (is (= type {:type 'int?}))
        form (a/ast->form ast)
        _ (is (matches? (let [?v (* in1 in1)]
                          (+ ?v ?v))
                        form))
        func (eval `(fn [~'in1] ~form))]
    (is (= (func 0) 0))
    (is (= (func 2) 8))
    (is (= (func -1) 2))))

(deftest hof-with-anonymous-fn-test
  ;; Map `inc` over the elements of a vector
  (let [{::c/keys [ast type]} (c/push->ast {:push      [{:gene :lit :val [1 2 3] :type {:type :vector :child {:type 'int?}}}
                                                        {:gene :fn :arg-types [lib/INT] :ret-type lib/INT}
                                                        [{:gene :local :idx 0}
                                                         {:gene :var :name 'int-inc}
                                                         {:gene :apply}]
                                                        {:gene :var :name 'map-vec}
                                                        {:gene :apply}]
                                            :locals    []
                                            :ret-type  {:type :vector :child {:type 'int?}}
                                            :type-env  lib/type-env
                                            :dealiases lib/dealiases})
        _ (is (= type {:type :vector :child {:type 'int?}}))
        form (a/ast->form ast)
        _ (is (matches? (mapv (fn [?a] (inc ?a)) [1 2 3])
                        form))
        func (eval `(fn [] ~form))]
    (is (= [2 3 4] (func)))))

(deftest nullary-fn-test
  ;; Generate a vector of 5 random doubles.
  (let [{::c/keys [ast type]} (c/push->ast {:push      [{:gene :var :name 'rand}
                                                        {:gene :apply}
                                                        {:gene :fn :ret-type lib/DOUBLE}
                                                        {:gene :lit :val 5 :type {:type 'int?}}
                                                        {:gene :var :name 'repeatedly}
                                                        {:gene :apply}]
                                            :locals    []
                                            :ret-type  {:type :vector :child {:type 'double?}}
                                            :type-env  {'rand       {:type   :=>
                                                                     :input  {:type :cat :children []}
                                                                     :output {:type 'double?}}
                                                        'repeatedly {:type   :scheme
                                                                     :s-vars ['a]
                                                                     :body   {:type   :=>
                                                                              :input  {:type     :cat
                                                                                       :children [{:type 'int?}
                                                                                                  {:type   :=>
                                                                                                   :input  {:type :cat :children []}
                                                                                                   :output {:type :s-var :sym 'a}}]}
                                                                              :output {:type  :vector
                                                                                       :child {:type :s-var :sym 'a}}}}}
                                            :dealiases {}})
        _ (is (= type {:type :vector :child {:type 'double?}}))
        form (a/ast->form ast)
        _ (is (= form '(repeatedly 5 (fn [] (rand)))))
        func (eval `(fn [] ~form))]
    (doseq [x (func)]
      (is (double? x)))))

(deftest side-effects-test
  ;; Print hello world and return 0
  (let [{::c/keys [ast type]} (c/push->ast {:push      [{:gene :lit :val 0 :type {:type 'int?}}
                                                        {:gene :lit :val "Hello world!" :type {:type 'string?}}
                                                        {:gene :var :name 'println}
                                                        {:gene :apply}
                                                        {:gene :var :name 'do2}
                                                        {:gene :apply}]
                                            :locals    []
                                            :ret-type  {:type 'int?}
                                            :type-env  lib/type-env
                                            :dealiases lib/dealiases})
        _ (is (= type {:type 'int?}))
        form (a/ast->form ast)
        _ (is (= form '(do (println "Hello world!") 0)))
        func (eval `(fn [] ~form))]
    (let [s (new StringWriter)]
      (binding [*out* s]
        (is (= (func) 0))
        (is (= (str s) "Hello world!\n"))))))

(deftest replace-space-with-newline-test
  (let [{::c/keys [ast type]} (c/push->ast {:push      [{:gene :lit :val \newline :type {:type 'char?}}
                                                        {:gene :lit :val \space :type {:type 'char?}}
                                                        {:gene :local :idx 0}
                                                        {:gene :var :name `lib/replace-char}
                                                        {:gene :apply}
                                                        {:gene :let}
                                                        [;; This vector contains the body of the `let`
                                                         ;; Starting with the second clause of the `do`
                                                         {:gene :lit :val \newline :type {:type 'char?}}
                                                         {:gene :local :idx 1}
                                                         {:gene :var :name `lib/remove-char}
                                                         {:gene :apply}
                                                         {:gene :var :name 'length}
                                                         {:gene :apply}
                                                         ;; Followed by the first clause of the `do`
                                                         {:gene :local :idx 1}
                                                         {:gene :var :name 'println}
                                                         {:gene :apply}
                                                         ;; Invoke the do
                                                         {:gene :var :name 'do2}
                                                         {:gene :apply}]]
                                            :locals    ['in1]
                                            :ret-type  {:type 'int?}
                                            :type-env  (assoc lib/type-env
                                                         'in1 {:type 'string?})
                                            :dealiases lib/dealiases})
        _ (is (= type {:type 'int?}))
        form (a/ast->form ast)
        _ (is (matches? (let [?v (erp12.cbgp-lite.lang.lib/replace-char in1 \space \newline)]
                          (do (println ?v)
                              (count (erp12.cbgp-lite.lang.lib/remove-char ?v \newline))))
                        form))
        func (eval `(fn [~'in1] ~form))]
    (let [s (new StringWriter)]
      (binding [*out* s]
        (is (= (func "a b c") 3))
        (is (= (str s) "a\nb\nc\n"))))))

(deftest polymorphic-output-test
  (let [{::c/keys [ast type]} (c/push->ast {:push      [{:gene :local :idx 0}
                                                        {:gene :local :idx 1}
                                                        {:gene :lit :val true :type {:type 'boolean?}}
                                                        {:gene :var :name 'if}
                                                        {:gene :apply}]
                                            :locals    ['x 'y ]
                                            :ret-type  (lib/s-var 'a)
                                            :type-env  (assoc lib/type-env
                                                         'x (lib/s-var 'a)
                                                         'y (lib/s-var 'a))
                                            :dealiases lib/dealiases})
        _ (is (= (:type type) :s-var))
        form (a/ast->form ast)
        _ (is (= form '(if true y x)))
        func (eval `(fn [~'x ~'y] ~form))]
    (is (= (func 0 1) 1))
    (is (= (func "this" "that") "that"))))
