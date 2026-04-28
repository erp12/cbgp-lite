(ns erp12.cbgp-lite.program.types-test
  (:import [clojure.lang ExceptionInfo])
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.cbgp-lite.program.types :as t]
            ;; adds support for `match?` and `thrown-match?` in `is` expressions
            [matcher-combinators.test]))

(declare match?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type checking/inference failure objects

(deftest fail-test
  (testing "fail with message"
    (is (= {::t/fail "error message"}
           (t/fail "error message"))))
  (testing "fail with message and data"
    (is (= {::t/fail "error message"
            ::t/data {:key "value"}}
           (t/fail "error message" {:key "value"})))))

(deftest fail?-test
  (is (t/fail? (t/fail "error")))
  (is (false? (t/fail? {})))
  (is (false? (t/fail? nil)))
  (is (false? (t/fail? 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type structure tests

(deftest type-var-test
  (let [tv (t/->TypeVar 'a :*)]
    (is (= 'a (:sym tv)))
    (is (= :* (:kind tv)))))

(deftest type-constructor-test
  (let [tc (t/->TypeConstructor 'INT :*)]
    (is (= 'INT (:sym tc)))
    (is (= :* (:kind tc)))))

(deftest type-app-test
  (let [con (t/->TypeConstructor 'LIST t/x->x)
        arg (t/->TypeVar 'a :*)
        app (t/->TypeApp con [arg])]
    (is (= con (:con app)))
    (is (= [arg] (:args app)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kind tests

(deftest kind-test
  (testing "TypeVar kind"
    (is (= :* (t/kind (t/->TypeVar 'a :*))))
    (is (= t/x->x (t/kind (t/->TypeVar 'hkt t/x->x)))))
  (testing "TypeConstructor kind"
    (is (= :* (t/kind t/INT)))
    (is (= t/x->x (t/kind t/LIST)))
    (is (= t/x->x->x (t/kind t/MAP))))
  (testing "TypeApp kind"
    (is (= :* (t/kind (t/->TypeApp t/LIST [t/INT]))))
    (is (= :* (t/kind (t/->TypeApp t/MAP [t/STRING t/INT])))))
  (testing "invalid TypeApp throws"
    (is (thrown-with-msg? ExceptionInfo #"Invalid TypeApp"
                          (t/kind (t/->TypeApp t/INT []))))))

(deftest kind-fn?-test
  (is (true? (t/kind-fn? t/x->x)))
  (is (true? (t/kind-fn? t/x->x->x)))
  (is (false? (t/kind-fn? :*)))
  (is (false? (t/kind-fn? nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Free type variables tests

(deftest free-type-vars-test
  (let [a (t/->TypeVar 'a :*)
        b (t/->TypeVar 'b :*)]
    (testing "TypeVar"
      (is (= #{a} (t/free-type-vars a))))
    (testing "TypeConstructor"
      (is (= #{} (t/free-type-vars t/INT))))
    (testing "TypeApp"
      (is (= #{a} (t/free-type-vars (t/->TypeApp t/LIST [a]))))
      (is (= #{a b} (t/free-type-vars (t/->TypeApp t/MAP [a b]))))
      (is (= #{b} (t/free-type-vars (t/->TypeApp (t/tuple-ctor 2) [t/INT b])))))
    (testing "GenVar"
      (is (= #{} (t/free-type-vars (t/->GenVar 0)))))
    (testing "Scheme"
      (is (= #{a b} (t/free-type-vars (t/->Scheme [:*]
                                                  (t/fn-type [(t/->GenVar 0) a] b))))))))

(deftest type-env-free-type-vars-test
  (let [a        (t/->TypeVar 'a :*)
        b        (t/->TypeVar 'b :*)
        type-env {'x a
                  'y b}]
    (is (= #{a b} (t/type-env-free-type-vars type-env))))
  (testing "empty type env"
    (is (= #{} (t/type-env-free-type-vars {})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitution tests

(deftest substitute-test
  (let [a    (t/->TypeVar 'a :*)
        b    (t/->TypeVar 'b :*)
        subs {a t/INT}]
    (testing "TypeVar substitution"
      (is (= t/INT (t/substitute a subs))))
    (testing "TypeVar not in subs"
      (is (= b (t/substitute b subs))))
    (testing "TypeConstructor unchanged"
      (is (= t/INT (t/substitute t/INT subs))))
    (testing "TypeApp substitution"
      (is (= (t/list-type t/INT)
             (t/substitute (t/list-type a) subs)))
      (is (= (t/map-type t/INT t/STRING)
             (t/substitute (t/map-type a b)
                           {a t/INT
                            b t/STRING}))))
    (testing "GenVar unchanged"
      (is (= (t/->GenVar 0) (t/substitute (t/->GenVar 0) subs))))
    (testing "Scheme substitution"
      (is (= (t/->Scheme [] t/INT)
             (t/substitute (t/->Scheme [] a) subs))))))

(deftest type-env-substitute-test
  (let [a        (t/->TypeVar 'a :*)
        b        (t/->TypeVar 'b :*)
        type-env {'x a
                  'y b}
        subs     {a t/INT}]
    (is (= {'x t/INT
            'y b}
           (t/type-env-substitute type-env subs)))))

(deftest compose-subs-test
  (let [a  (t/->TypeVar 'a :*)
        b  (t/->TypeVar 'b :*)
        c  (t/->TypeVar 'c :*)
        s1 {a b}
        s2 {b c}]
    (testing "compose s1 then s2"
      (is (= {a c
              b c}
             (t/compose-subs s1 s2))))
    (testing "s1 takes precedence"
      (let [s1 {a t/INT}
            s2 {a t/STRING}]
        (is (= {a t/INT}
               (t/compose-subs s1 s2)))))
    (testing "empty subs"
      (is (= {a b} (t/compose-subs {} {a b})))
      (is (= {a b} (t/compose-subs {a b} {}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instantiation tests

(deftest instantiate-test
  (let [a         (t/->GenVar 0)
        b         (t/->GenVar 1)
        type-args [(t/->TypeVar 'x :*) (t/->TypeVar 'y :*)]]
    (testing "GenVar instantiation"
      (is (= (first type-args) (t/instantiate a type-args)))
      (is (= (second type-args) (t/instantiate b type-args))))
    (testing "TypeVar unchanged"
      (is (= (t/->TypeVar 'a :*)
             (t/instantiate (t/->TypeVar 'a :*) type-args))))
    (testing "TypeConstructor unchanged"
      (is (= t/INT (t/instantiate t/INT type-args))))
    (testing "TypeApp instantiation"
      (is (= (t/->TypeApp t/LIST [(first type-args)])
             (t/instantiate (t/->TypeApp t/LIST [a]) type-args))))))

(deftest instantiate-scheme-test
  (is (= t/STRING
         (t/instantiate-scheme (t/->Scheme [] t/STRING))))
  (is (match? (t/fn-type [(t/->TypeVar symbol? :*) (t/->TypeVar symbol? :*)]
                         (t/->TypeVar symbol? :*))
              (t/instantiate-scheme (t/->Scheme [:* :*]
                                                (t/fn-type [(t/->GenVar 0) (t/->GenVar 1)] (t/->GenVar 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructor helper tests

(deftest list-type-test
  (is (= (t/->TypeApp t/LIST [t/INT])
         (t/list-type t/INT)))
  (is (= (t/->TypeApp t/LIST [(t/->TypeApp t/LIST [t/STRING])])
         (t/list-type (t/list-type t/STRING)))))

(deftest vec-type-test
  (is (= (t/->TypeApp t/VECTOR [t/INT])
         (t/vec-type t/INT)))
  (is (= (t/->TypeApp t/VECTOR [t/STRING])
         (t/vec-type t/STRING))))

(deftest set-type-test
  (is (= (t/->TypeApp t/SET [t/INT])
         (t/set-type t/INT)))
  (is (= (t/->TypeApp t/SET [t/STRING])
         (t/set-type t/STRING))))

(deftest map-type-test
  (is (= (t/->TypeApp t/MAP [t/STRING t/INT])
         (t/map-type t/STRING t/INT)))
  (is (= (t/->TypeApp t/MAP [t/INT t/STRING])
         (t/map-type t/INT t/STRING))))

(deftest fn-type-test
  (is (= (t/->TypeApp (t/fn-ctor 2) [t/INT t/STRING t/BOOL])
         (t/fn-type [t/INT t/STRING] t/BOOL)))
  (is (= (t/->TypeApp (t/fn-ctor 0) [t/INT])
         (t/fn-type [] t/INT)))
  (is (= (t/->TypeApp (t/fn-ctor 1) [t/INT t/STRING])
         (t/fn-type [t/INT] t/STRING))))

(deftest tuple-type-test
  (is (= (t/->TypeApp (t/tuple-ctor 3) [t/INT t/STRING t/BOOL])
         (t/tuple-type [t/INT t/STRING t/BOOL])))
  (is (= (t/->TypeApp (t/tuple-ctor 1) [t/INT])
         (t/tuple-type [t/INT]))))

(deftest fn-type?-test
  (is (t/fn-type? (t/fn-type [t/INT t/STRING] t/BOOL)))
  (is (false? (t/fn-type? (t/list-type t/INT))))
  (is (false? (t/fn-type? t/INT))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unification tests

(deftest bind-var-test
  (let [a (t/->TypeVar 'a :*)]
    (testing "bind to simple type"
      (is (= {a t/INT} (t/bind-var a t/INT))))
    (testing "bind to same var"
      (is (= {} (t/bind-var a a))))
    (testing "occurs check failure"
      (let [list-a (t/list-type a)]
        (is (t/fail? (t/bind-var a list-a)))))
    (testing "kind mismatch failure"
      (let [wrong-kind (t/->TypeVar 'b t/x->x)]
        (is (t/fail? (t/bind-var a wrong-kind)))))))

(deftest mgu-test
  (let [a (t/->TypeVar 'a :*)
        b (t/->TypeVar 'b :*)
        c (t/->TypeVar 'c :*)]
    (testing "TypeVar with TypeVar"
      (is (= {a b} (t/mgu a b))))
    (testing "TypeVar with TypeConstructor"
      (is (= {a t/INT} (t/mgu a t/INT))))
    (testing "TypeConstructor with TypeConstructor"
      (is (= {} (t/mgu t/INT t/INT)))
      (is (t/fail? (t/mgu t/INT t/STRING))))
    (testing "TypeApp with TypeApp - same constructor"
      (is (= {a t/INT} (t/mgu (t/list-type a) (t/list-type t/INT))))
      (is (t/fail? (t/mgu (t/list-type t/INT) (t/list-type t/STRING)))))
    (testing "TypeApp with TypeApp - different constructors"
      (is (t/fail? (t/mgu (t/list-type t/INT) (t/vec-type t/INT)))))
    (testing "TypeApp with different arity"
      (is (t/fail? (t/mgu (t/map-type t/INT t/STRING)
                          (t/list-type t/INT)))))
    (testing "complex unification"
      (is (= {a t/INT
              b t/STRING
              c t/INT}
             (t/mgu (t/map-type a (t/fn-type [b] c))
                    (t/map-type t/INT (t/fn-type [t/STRING] t/INT))))))))

(deftest with-unifier-test
  (let [a (t/->TypeVar 'a :*)
        b (t/->TypeVar 'b :*)
        c (t/->TypeVar 'c :*)]
    (testing "success case"
      (is (= {a b}
             (t/with-unifier {} a b))))
    (testing "with existing substitutions"
      (is (= {a b
              c t/STRING}
             (t/with-unifier {c t/STRING} a b))))
    (testing "failure case"
      (is (t/fail? (t/with-unifier {} t/INT t/STRING))))))

(deftest unifiable?-test
  (testing "unifiable types"
    (is (t/unifiable? t/INT t/INT))
    (is (t/unifiable? (t/->TypeVar 'a :*) t/INT))
    (is (t/unifiable? (t/list-type (t/->TypeVar 'a :*))
                      (t/list-type t/INT))))
  (testing "non-unifiable types"
    (is (false? (t/unifiable? t/INT t/STRING)))
    (is (false? (t/unifiable? (t/list-type t/INT)
                              (t/vec-type t/INT))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme tests

(deftest quantify-test
  (let [a      (t/->TypeVar 'a :*)
        b      (t/->TypeVar 'b :*)
        typ    (t/fn-type [a b] a)
        t-vars #{a}]
    (is (= (t/->Scheme [:*] (t/fn-type [(t/->GenVar 0) b] (t/->GenVar 0)))
           (t/quantify t-vars typ)))))

(deftest generalize-test
  (let [a        (t/->TypeVar 'a :*)
        b        (t/->TypeVar 'b :*)
        type-env {'x a}
        typ      (t/fn-type [a b] a)]
    (is (= (t/->Scheme [:*] (t/fn-type [a (t/->GenVar 0)] a))
           (t/generalize typ type-env))))
  (testing "no free vars to generalize"
    (let [type-env {'x t/INT}
          typ      t/INT]
      (is (= (t/->Scheme [] t/INT)
             (t/generalize typ type-env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sugar tests

(deftest fn-scheme-test
  (is (match? (t/->Scheme [] (t/fn-type [t/INT t/STRING] t/BOOL))
              (t/fn-scheme {:args [t/INT t/STRING]
                            :ret  t/BOOL})))
  (is (match? (t/->Scheme [:*] (t/fn-type [(t/->GenVar 0)] t/INT))
              (t/fn-scheme {:kinds [:*]
                            :args  [0]
                            :ret   t/INT}))))

(deftest type-ctors-test
  (testing "simple type"
    (is (= #{t/INT} (t/type-ctors t/INT))))
  (testing "list type"
    (is (= #{t/LIST t/INT} (t/type-ctors (t/list-type t/INT)))))
  (testing "function type"
    (is (= #{(t/fn-ctor 2) t/INT t/STRING t/BOOL}
           (t/type-ctors (t/fn-type [t/INT t/STRING] t/BOOL)))))
  (testing "nested type"
    (is (= #{t/LIST t/VECTOR t/INT}
           (t/type-ctors (t/list-type (t/vec-type t/INT)))))))