(ns erp12.cbgp-lite.program.compile-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.cbgp-lite.genome :as g]
            [erp12.cbgp-lite.program.compile :as c]
            [erp12.cbgp-lite.program.expr :as e]
            [erp12.cbgp-lite.program.lib :as lib]
            [erp12.cbgp-lite.program.types :as t]
            ;; adds support for `match?` and `thrown-match?` in `is` expressions
            [matcher-combinators.test])
  (:import (clojure.lang ExceptionInfo)
           (java.io StringWriter)))

(declare match?)

(def test-asts
  {:1   (c/->Ast (e/->Lit 1 t/INT) t/INT)
   :str (c/->Ast (e/->Var 'input) t/STRING)
   :+   (c/->Ast (e/->Var `lib/int-add)
                 (t/instantiate-scheme (lib/type-env `lib/int-add)))
   :do  (c/->Ast (e/->Var 'do)
                 (t/instantiate-scheme (lib/type-env 'do)))})

(def test-state
  {:asts   (list (:do test-asts) (:str test-asts) (:1 test-asts) (:+ test-asts))
   :locals {}})

(deftest push-ast-test
  (testing "push to empty stack"
    (let [result (c/push-ast (:1 test-asts) c/empty-state)]
      (is (= 1 (count (:asts result))))
      (is (= (:1 test-asts) (first (:asts result))))))
  (testing "push to non-empty stack"
    (is (= (list (:1 test-asts) (:do test-asts) (:str test-asts) (:1 test-asts) (:+ test-asts))
           (:asts (c/push-ast (:1 test-asts) test-state))))))

(deftest pop-ast-test
  (testing "pop from empty state"
    (let [result (c/pop-ast c/empty-state)]
      (is (= :none (:ast result)))
      (is (= c/empty-state (:state result)))))
  (testing "pop single AST - skip special vars"
    (let [result (c/pop-ast test-state)]
      (is (= (:str test-asts) (:ast result)))
      (is (=  (list (:do test-asts) (:1 test-asts) (:+ test-asts))
              (:asts (:state result)))))))

(deftest pop-unifiable-ast-test
  (testing "pop from empty state"
    (let [result (c/pop-unifiable-ast t/INT c/empty-state)]
      (is (= :none (:ast result)))
      (is (= c/empty-state (:state result)))
      (is (= {} (:substitutions result)))))
  (testing "pop AST with exact type match"
    (let [result (c/pop-unifiable-ast t/INT test-state)]
      (is (= (:1 test-asts) (:ast result)))
      (is (= (list (:do test-asts) (:str test-asts) (:+ test-asts))
             (:asts (:state result))))))
  (testing "pop AST unifiable with type variable"
    (let [t      (t/->TypeVar 't :*)
          result (c/pop-unifiable-ast t test-state)]
      (is (= (:str test-asts) (:ast result)))
      (is (= (list (:do test-asts) (:1 test-asts) (:+ test-asts))
             (:asts (:state result))))
      (is (= {t t/STRING} (:substitutions result)))))
  (testing "no unifiable ASTs"
    (is (= {:ast           :none
            :state         test-state
            :substitutions {}}
           (c/pop-unifiable-ast (t/vec-type t/BOOL) test-state))))
  (testing "pop unifiable function AST"
    (let [t         (t/->TypeVar 't :*)
          poly-func (t/fn-type [t t] t)
          result    (c/pop-unifiable-ast poly-func test-state)]
      (is (= (:+ test-asts) (:ast result)))
      (is (= (list (:do test-asts) (:str test-asts) (:1 test-asts))
             (:asts (:state result)))))))

(deftest pop-function-ast-test
  (testing "pop from empty state"
    (let [result (c/pop-function-ast c/empty-state)]
      (is (= :none (:ast result)))
      (is (= c/empty-state (:state result)))))
  (testing "pop function AST"
    (let [result1 (c/pop-function-ast test-state)
          result2 (c/pop-function-ast (:state result1))]
      ;; Special vars are allowed when popping an AST to call as a function.
      (is (= (:do test-asts) (:ast result1)))
      (is (= (:+ test-asts) (:ast result2)))
      (is (= (list (:str test-asts) (:1 test-asts))
             (:asts (:state result2)))))))

(deftest lit-compile-step-test
  (is (= {:asts   (list (c/->Ast (e/->Lit 0 t/INT) t/INT))
          :push   (list)
          :locals []}
         (c/compile-step (g/->Lit 0 t/INT)
                         c/empty-state
                         lib/type-env
                         {}))))

(deftest var-compile-step-test
  (testing "var with monomorphic type"
    (is (= {:asts   (list (c/->Ast (e/->Var 'x) t/INT))
            :push   (list)
            :locals []}
           (c/compile-step (g/->Var 'x)
                           c/empty-state
                           {'x (t/->Scheme [] t/INT)}
                           {}))))
  (testing "var with polymorphic type"
    (is (match? {:asts   (list (c/->Ast (e/->Var 'f)
                                        (t/fn-type [(t/->TypeVar symbol? :*)
                                                    (t/->TypeVar symbol? :*)]
                                                   (t/->TypeVar symbol? :*))))
                 :push   (list)
                 :locals []}
                (c/compile-step (g/->Var 'f)
                                c/empty-state
                                {'f (t/fn-scheme {:kinds [:*]
                                                  :args  [0 0]
                                                  :ret   0})}
                                {}))))
  (testing "missing var"
    (is (thrown? ExceptionInfo
                 (c/compile-step (g/->Var 'not-found)
                                 c/empty-state
                                 {}
                                 {})))))

(deftest local-compile-step-test
  (testing "resolve to monomorphic var"
    (is (= {:asts   (list (c/->Ast (e/->Var 'x) t/INT))
            :push   (list)
            :locals ['x]}
           (c/compile-step (g/->Local 7)
                           (assoc c/empty-state :locals ['x])
                           {'x (t/->Scheme [] t/INT)}
                           {}))))
  (testing "resolve to polymorphic function"
    (is (match? {:asts   (list (c/->Ast (e/->Var 'f)
                                        (t/fn-type [(t/->TypeVar symbol? :*)]
                                                   (t/->TypeVar symbol? :*))))
                 :push   (list)
                 :locals ['f]}
                (c/compile-step (g/->Local 100)
                                (assoc c/empty-state :locals ['f])
                                {'f (t/fn-scheme {:kinds [:*]
                                                  :args  [0]
                                                  :ret   0})}
                                {}))))
  (testing "empty locals"
    (is (= {:asts   (list)
            :push   (list)
            :locals []}
           (c/compile-step (g/->Local 100)
                           c/empty-state
                           {'x (t/->Scheme [] t/INT)}
                           {}))))
  (testing "missing local in type-env"
    (is (thrown? ExceptionInfo
                 (c/compile-step (g/->Local 0)
                                 (assoc c/empty-state :locals ['f])
                                 {}
                                 {})))))

(deftest apply-compile-step-test
  (testing "monomorphic function application"
    (is (= {:asts (list (c/->Ast (e/->App (e/->Var `lib/int-add)
                                          [(e/->Lit 1 t/INT)
                                           (e/->Lit 2 t/INT)])
                                 t/INT))}
           (c/compile-step (g/->App)
                           {:asts (list (c/->Ast (e/->Lit 1 t/INT) t/INT)
                                        (c/->Ast (e/->Var `lib/int-add) (t/instantiate-scheme (lib/type-env `lib/int-add)))
                                        (c/->Ast (e/->Lit 2 t/INT) t/INT))}
                           lib/type-env
                           {}))))
  (testing "polymorphic function application"
    (let [t-var   (t/->TypeVar 'T :*)]
      (is (= {:asts   (list (c/->Ast (e/->App (e/->Var 'id)
                                              [(e/->Lit 1 t/INT)])
                                     t/INT))
              :locals []}
             (c/compile-step (g/->App)
                             {:asts   (list (c/->Ast (e/->Var 'id) (t/fn-type [t-var] t-var))
                                            (c/->Ast (e/->Lit 1 t/INT) t/INT))
                              :locals []}
                             {}
                             {})))))
  (testing "no function noop"
    (let [state {:asts   (list (c/->Ast (e/->Lit 1 t/INT) t/INT))
                 :locals []}]
      (is (= state
             (c/compile-step (g/->App)
                             state
                             {}
                             {})))))
  (testing "no arg noop"
    (let [fn-ast (c/->Ast (e/->Var 'inc) (t/fn-type [t/INT] t/INT))
          state  {:asts   (list fn-ast)
                  :locals []}]
      (is (= state
             (c/compile-step (g/->App)
                             state
                             {}
                             {}))))))

(deftest abs-compile-step-test
  (testing "create a nullary anonymous function"
    (is (= {:asts   (list (c/->Ast (e/->Abs [] (e/->Lit 1 t/INT))
                                   (t/fn-type [] t/INT)))
            :locals []}
           (c/compile-step (assoc (g/->Abs [] t/INT) :param-symbols [])
                           {:asts   (list (:1 test-asts))
                            :locals []}
                           {}
                           {}))))
  (testing "create a 2 argument monomorphic anonymous function"
    (is (= {:asts   (list (c/->Ast (e/->Abs ['a 'b] (e/->Var 'a))
                                   (t/fn-type [t/INT t/INT] t/INT)))
            :push   (list)
            :locals []}
           (c/compile-step (assoc (g/->Abs [t/INT t/INT] t/INT)
                                  :param-symbols ['a 'b]
                                  :push [(g/->Local 0)])
                           c/empty-state
                           {}
                           {}))))
  (testing "create a polymorphic anonymous function"
    ;; Identity function: T -> T
    (let [t-var (t/->TypeVar 'T :*)]
      (is (= {:asts   (list (c/->Ast (e/->Abs ['a] (e/->Var 'a))
                                     (t/fn-type [t-var] t-var)))
              :push   (list)
              :locals []}
             (c/compile-step (assoc (g/->Abs [t-var] t-var)
                                    :param-symbols ['a]
                                    :push [(g/->Local 0)])
                             c/empty-state
                             {}
                             {}))))
    ;; Allow the body of the function to constrain the lambda type.
    (let [t-var (t/->TypeVar 'T :*)]
      ;; @todo should the compiled AST be (T -> INT) or (INT -> INT)?
      (is (= {:asts   (list (c/->Ast (e/->Abs ['a] (e/->Lit 0 t/INT))
                                     (t/fn-type [t-var] t/INT)))
              :push   (list)
              :locals []}
             (c/compile-step (assoc (g/->Abs [t-var] t-var)
                                    :param-symbols ['a]
                                    :push [(g/->Lit 0 t/INT)])
                             c/empty-state
                             {}
                             {})))))
  (testing "no valid function body ast"
    (is (= c/empty-state
           (c/compile-step (g/->Abs [] t/INT)
                           c/empty-state
                           lib/type-env
                           {})))
    (is (= c/empty-state
           (c/compile-step (assoc (g/->Abs [t/INT] t/INT)
                                  :push [])
                           c/empty-state
                           lib/type-env
                           {})))))

(deftest let-compile-step-test
  (testing "monomorphic local in Let"
    (is (= {:asts   (list (c/->Ast (e/->Let [{:sym 'x
                                              :def (e/->Lit 1 t/INT)}]
                                            (e/->Var 'x))
                                   t/INT))
            :locals []}
           (c/compile-step (assoc (g/->Let)
                                  :sym 'x
                                  :push [(g/->Local 0)])
                           {:asts   (list (:1 test-asts))
                            :locals []}
                           lib/type-env
                           {}))))
  (testing "polymorphic local in Let"
    (let [t-var  (t/->TypeVar (gensym "t-") :*)]
      (is (match? {:asts   (list (c/->Ast (e/->Let [{:sym 'x
                                                     :def (e/->Var 'id)}]
                                                   (e/->Var 'x))
                                          ;; Type should be a new instantiation of the polymorphic function.
                                          (t/fn-type [(t/->TypeVar symbol? :*)]
                                                     (t/->TypeVar symbol? :*))))
                   :locals []}
                  (c/compile-step (assoc (g/->Let) 
                                         :sym 'x 
                                         :push [(g/->Local 0)])
                                  {:asts   (list (c/->Ast (e/->Var 'id)
                                                          (t/fn-type [t-var] t-var)))
                                   :locals []}
                                  lib/type-env
                                  {})))))
  (testing "No def on stack"
    (is (= c/empty-state
           (c/compile-step (g/->Let)
                           c/empty-state
                           lib/type-env
                           {}))))
  (testing "No body on stack"
    (let [state {:asts (list (:1 test-asts))}]
      (is (= state
             (c/compile-step (assoc (g/->Let) :push [])
                             state
                             lib/type-env
                             {}))))))

(deftest push->ast-test
  (testing "compiling a polymorphic HOF"
    (is (match? (c/->Ast (e/->App (e/->Var 'identity) [(e/->Var 'poly-f)])
                         (t/fn-type [(t/->TypeVar symbol? :*)] (t/->TypeVar symbol? :*)))
                (c/push->ast {:push        (list (g/->Var 'poly-f)
                                                 (g/->Var 'identity)
                                                 (g/->App))
                              :output-type (t/fn-type [(t/fresh-type-var :*)] (t/fresh-type-var :*))
                              :type-env    {'identity (t/fn-scheme {:kinds [:*]
                                                                    :args  [0]
                                                                    :ret   0})
                                            'poly-f   (t/fn-scheme {:kinds [:* :*]
                                                                    :args  [0]
                                                                    :ret   1})}}))))
  (testing "compiling a polymorphic thunk"
    (let [gene-tvar (t/fresh-type-var :*)
          output-tvar (t/fresh-type-var :*)]
      (is (match? (c/->Ast (e/->Abs [] (e/->Var 'identity))
                           (t/fn-type [] (t/fn-type [(t/->TypeVar symbol? :*)] (t/->TypeVar symbol? :*))))
                  (c/push->ast {:push        (list (g/->Var 'identity)
                                                   (g/->Abs [] (t/fn-type [gene-tvar] gene-tvar)))
                                :output-type (t/fn-type [] (t/fn-type [output-tvar] output-tvar))
                                :type-env    {'identity (t/fn-scheme {:kinds [:*]
                                                                      :args  [0]
                                                                      :ret   0})}})))))
  (testing "compiling simple math - (lib/int-add in1 100)"
    (let [ast  (c/push->ast {:push        (list (g/->Lit 100 t/INT)
                                                (g/->Local 0)
                                                (g/->Var `lib/int-add)
                                                (g/->App))
                             :locals      ['in1]
                             :output-type t/INT
                             :type-env    (assoc lib/type-env 'in1 (t/->Scheme [] t/INT))})
          form (e/to-form (:expr ast))
          func (e/form->fn ['in1] form)]
      (is (= t/INT (:typ ast)))
      (is (= 100 (func 0)))
      (is (= 101 (func 1)))))
  (testing "compiling conditional logic - (if (lib/<' in1 1000) small large)"
    (let [ast  (c/push->ast {:push        (list (g/->Lit "large" t/STRING)
                                                (g/->Lit "small" t/STRING)
                                                (g/->Lit 1000 t/INT)
                                                (g/->Local 0)
                                                (g/->Var `lib/<')
                                                (g/->App)
                                                (g/->Var 'if)
                                                (g/->App))
                             :locals      ['in1]
                             :output-type t/STRING
                             :type-env    (assoc lib/type-env 'in1 (t/->Scheme [] t/INT))})
          form (e/to-form (:expr ast))
          func (e/form->fn ['in1] form)]
      (is (= t/STRING (:typ ast)))
      (is (= "small" (func 0)))
      (is (= "large" (func 1000)))
      (is (= "large" (func 2000)))))
  (testing "compiling let forms - (let [x (lib/int-mult in1 in1)] (lib/int-add x x))"
    (let [ast  (c/push->ast {:push        (list (g/->Local 0)
                                                (g/->Local 0)
                                                (g/->Var `lib/int-mult)
                                                (g/->App)
                                                (assoc (g/->Let)
                                                       :sym 'x
                                                       :push (list (g/->Local 1)
                                                                   (g/->Local 1)
                                                                   (g/->Var `lib/int-add)
                                                                   (g/->App))))
                             :locals      ['in1]
                             :output-type t/INT
                             :type-env    (assoc lib/type-env 'in1 (t/->Scheme [] t/INT))})
          form (e/to-form (:expr ast))
          func (e/form->fn ['in1] form)]
      (is (= t/INT (:typ ast)))
      (is (= 0 (func 0)))
      (is (= 8 (func 2)))
      (is (= 2 (func -1)))))
  (testing "compiling HOF with anonymous function - (mapv (fn [x] (lib/int-inc x)) [1 2 3])"
    (let [ast  (c/push->ast {:push        (list (g/->Lit [1 2 3] (t/vec-type t/INT))
                                                (assoc (g/->Abs [t/INT] t/INT)
                                                       :param-symbols ['x]
                                                       :push (list (g/->Local 0)
                                                                   (g/->Var `lib/int-inc)
                                                                   (g/->App)))
                                                (g/->Var `mapv)
                                                (g/->App))
                             :output-type (t/vec-type t/INT)
                             :type-env    lib/type-env})
          form (e/to-form (:expr ast))
          func (e/form->fn [] form)]
      (is (= (t/vec-type t/INT) (:typ ast)))
      (is (= [2 3 4] (func)))))
  (testing "compiling nullary functions - (repeatedly 5 (fn [] (rand)))"
    (let [ast  (c/push->ast {:push        (list (g/->Var 'rand)
                                                (g/->App)
                                                (g/->Abs [] t/FLOAT)
                                                (g/->Lit 5 t/INT)
                                                (g/->Var 'repeatedly)
                                                (g/->App))
                             :output-type (t/vec-type t/FLOAT)
                             :type-env    {'rand       (t/fn-scheme {:args [] :ret t/FLOAT})
                                           'repeatedly (t/fn-scheme {:kinds [:*]
                                                                     :args  [t/INT (t/fn-type [] 0)]
                                                                     :ret   (t/vec-type 0)})}})
          form (e/to-form (:expr ast))
          func (e/form->fn [] form)]
      (is (= (t/vec-type t/FLOAT) (:typ ast)))
      (doseq [x (func)]
        (is (double? x)))))
  (testing "compiling functions with side effects - (do (println \"Hello world!\") 0)"
    (let [ast  (c/push->ast {:push        (list (g/->Lit 0 t/INT)
                                                (g/->Lit "Hello world!" t/STRING)
                                                (g/->Var `println)
                                                (g/->App)
                                                (g/->Var 'do)
                                                (g/->App))
                             :output-type t/INT
                             :type-env    lib/type-env})
          form (e/to-form (:expr ast))
          func (e/form->fn [] form)
          s    (new StringWriter)]
      (is (= t/INT (:typ ast)))
      (binding [*out* s]
        (is (= 0 (func)))
        (is (= "Hello world!\n" (str s))))))
  (testing "compiling replace-space-with-newline"
    (let [ast  (c/push->ast {:push        (list (g/->Lit \newline t/CHAR)
                                                (g/->Lit \space t/CHAR)
                                                (g/->Local 0)
                                                (g/->Var `lib/replace-char)
                                                (g/->App)
                                                (assoc (g/->Let)
                                                       :sym 'x
                                                       :push (list (g/->Lit \newline t/CHAR)
                                                                   (g/->Local 1)
                                                                   (g/->Var `lib/remove-char)
                                                                   (g/->App)
                                                                   (g/->Var `lib/str-count)
                                                                   (g/->App)
                                                                   (g/->Local 1)
                                                                   (g/->Var `println)
                                                                   (g/->App)
                                                                   (g/->Var 'do)
                                                                   (g/->App))))
                             :locals      ['in1]
                             :output-type t/INT
                             :type-env    (assoc lib/type-env 'in1 (t/->Scheme [] t/STRING))})
          form (e/to-form (:expr ast))
          func (e/form->fn ['in1] form)
          s    (new StringWriter)]
      (is (= t/INT (:typ ast)))
      (binding [*out* s]
        (is (= 3 (func "a b c")))
        (is (= "a\nb\nc\n" (str s)))))))

(deftest polymorphic-output-test
  (testing "compiling a program with polymorphic signature"
    (let [A    (t/rigid 'A)
          ast  (c/push->ast {:push        (list (g/->Local 0)
                                                (g/->Local 1)
                                                (g/->Lit true t/BOOL)
                                                (g/->Var 'if)
                                                (g/->App))
                             :output-type A
                             :locals      ['x 'y]
                             :type-env    (assoc lib/type-env
                                                 'x (t/->Scheme [] A)
                                                 'y (t/->Scheme [] A))})
          _    (is (= (:typ ast) A))
          form (e/to-form (:expr ast))
          _    (is (= form '(if true y x)))
          func (e/form->fn ['x 'y] form)]
      (is (= (func 0 1) 1))
      (is (= (func "this" "that") "that"))))
  (testing "compiling a polymorphic program with polymorphic lambda and let"
    (let [T1 (t/rigid 'T1)
          T2 (t/rigid 'T2)
          ast (c/push->ast {:push        (list (assoc (g/->Abs [T2] T2)
                                                      :param-symbols ['a]
                                                      :push (list (g/->Local 0)))
                                               (assoc (g/->Let)
                                                      :sym 'l
                                                      :push (list (g/->Var 'input)
                                                                  (g/->Local 0)
                                                                  (g/->App))))
                            :output-type T1
                            :type-env    {'input (t/->Scheme [] T1)}})
          _ (is (= (:typ ast) T1))
          form (e/to-form (:expr ast))
          func (e/form->fn ['input] form)]
      (is (= (func 1) 1))
      (is (= (func :a) :a)))))

