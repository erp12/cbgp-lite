(ns erp12.cbgp-lite.program.types
  "A Hindley-Milner type system for lambda-calculus like expressions."
  (:require [clojure.set :refer [union difference]]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk]]))

(defn fail
  "Create a map denoting some kind of type checking/inference failure."
  ([msg] {::fail msg})
  ([msg data] {::fail msg ::data data}))

(defn fail? [x] (and (map? x) (contains? x ::fail)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Structures

(defrecord TypeVar [sym kind])

(defn fresh-type-var
  [kind]
  (->TypeVar (gensym "t-") kind))

(defrecord TypeConstructor [sym kind])
(defrecord TypeApp [con args])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme

(defrecord GenVar [index])
(defrecord Scheme [kinds typ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kinds

(def x->x {:k-args [:*], :k-ret :*})
(def x->x->x {:k-args [:* :*], :k-ret :*})

(defn kind-fn? [{:keys [k-args k-ret]}] (boolean (and k-args k-ret)))

(defprotocol HasKind
  (kind [this] "The data type's kind."))

(extend-protocol HasKind
  TypeVar
  (kind [this] (:kind this))

  TypeConstructor
  (kind [this] (:kind this))

  TypeApp
  (kind [this]
    (if (and (satisfies? HasKind (:con this))
             ;; The kind of the type constructor must take type arguments...
             (kind-fn? (kind (:con this)))
             ;; ... and we must have the correct number of type arguments.
             (= (count (:k-args (kind (:con this))))
                (count (:args this)))
             ;; Every type argument must have a kind.
             (every? #(satisfies? HasKind %)
                     (:args this)))
      (:k-ret (kind (:con this)))
      (throw (ex-info "Invalid TypeApp" this)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rigid (Skolem) Type Var

(defn rigid
  "Creates a type variable that cannot be unified with anything other than itself."
  [sym]
  (assoc (->TypeVar sym :*) :rigid? true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Free Type Vars

(defprotocol FreeTypeVars
  (free-type-vars [this] "The set of free type vars in this structure."))

(extend-protocol FreeTypeVars
  TypeVar
  (free-type-vars [this] #{this})

  TypeConstructor
  (free-type-vars [this] #{})

  TypeApp
  (free-type-vars [this]
    (reduce (fn [ftv arg] (union ftv (free-type-vars arg)))
            (free-type-vars (:con this))
            (:args this)))

  GenVar
  (free-type-vars [this] #{})

  Scheme
  (free-type-vars [this] (free-type-vars (:typ this))))

(defn type-env-free-type-vars
  [type-env]
  (reduce (fn [result typ]
            (union result (free-type-vars typ)))
          #{}
          (vals type-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitutions

(defprotocol Substitutable
  (substitute [this substitutions] "Applies the given substitutions to this structure."))

(extend-protocol Substitutable
  TypeVar
  (substitute [this substitutions] (get substitutions this this))

  TypeConstructor
  (substitute [this substitutions] this)

  TypeApp
  (substitute [this substitutions]
    (->TypeApp (substitute (:con this) substitutions)
               (mapv #(substitute % substitutions) (:args this))))

  GenVar
  (substitute [this substitutions] this)

  Scheme
  (substitute [this substitutions]
    (->Scheme (:kinds this)
              (substitute (:typ this) substitutions))))

(defn type-env-substitute
  "Applies the substitution to each type in the type environment."
  [type-env substitutions]
  (update-vals type-env #(substitute % substitutions)))

(defn compose-subs
  "Creates substitutions equivalent to applying s1 followed by s2."
  [s1 s2]
  (reduce-kv
   (fn [acc t-var typ]
     (assoc acc t-var (substitute typ s2)))
   s2
   s1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Head Normal Form

(defprotocol Instantiatable
  (instantiate [this type-args] "Replaces each GenVar with the corresponding TypeVar in type-args."))

(extend-protocol Instantiatable
  TypeVar
  (instantiate [this type-args] this)

  TypeConstructor
  (instantiate [this type-args] this)

  TypeApp
  (instantiate [this type-args]
    (->TypeApp (instantiate (:con this) type-args)
               (mapv #(instantiate % type-args) (:args this))))

  GenVar
  (instantiate [this type-args]
    (nth type-args (:index this))))

(defn instantiate-scheme
  "Instantiates the Scheme by replacing each unique GenVar with a fresh TypeVar."
  [scheme]
  (instantiate (:typ scheme)
               (mapv fresh-type-var (:kinds scheme))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Type Constructors
(def NIL (->TypeConstructor 'NIL :*))
(def BOOL (->TypeConstructor 'BOOL :*))
(def INT (->TypeConstructor 'INT :*))
(def FLOAT (->TypeConstructor 'FLOAT :*))
(def CHAR (->TypeConstructor 'CHAR :*))
(def STRING (->TypeConstructor 'STRING :*))
(def KEYWORD (->TypeConstructor 'KEYWORD :*))
(def LIST (->TypeConstructor 'LIST x->x))
(def VECTOR (->TypeConstructor 'VECTOR x->x))
(def SET (->TypeConstructor 'SET x->x))
(def MAP (->TypeConstructor 'MAP x->x->x))

(defn fn-ctor
  "Creates a function type constructor for the given arity."
  [arity]
  (->TypeConstructor (symbol (str "FUNCTION" arity))
                     {:k-args (vec (repeat (inc arity) :*))
                      :k-ret :*}))

(defn tuple-ctor
  "Create a tuple type constructor for tuples with the given number of elements."
  [arity]
  (->TypeConstructor (symbol (str "TUPLE" arity))
                     {:k-args (vec (repeat arity :*))
                      :k-ret :*}))

;; Helpers

(defn list-type
  [el-type]
  (->TypeApp LIST [el-type]))

(defn vec-type
  [el-type]
  (->TypeApp VECTOR [el-type]))

(defn set-type
  [el-type]
  (->TypeApp SET [el-type]))

(defn map-type
  [key-type val-type]
  (->TypeApp MAP [key-type val-type]))

(defn fn-type
  [arg-types ret-type]
  (->TypeApp (fn-ctor (count arg-types))
             (conj (vec arg-types) ret-type)))

(defn tuple-type
  [el-types]
  (->TypeApp (tuple-ctor (count el-types))
             (vec el-types)))

(defn fn-type?
  "True if the given type is a function type. False otherwise."
  [typ]
  (and (instance? TypeApp typ)
       (str/starts-with? (name (:sym (:con typ)))
                         "FUNCTION")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unification

(defn bind-var
  "Creates a substitution from the type var to the type, if possible."
  [t-var typ]
  (cond
    (= t-var typ) {}

    (:rigid? t-var)
    (if (and (instance? TypeVar typ)
             (not (:rigid? typ)))
      (bind-var typ t-var)
      (fail "Cannot unify rigid type variables." {:t-var t-var
                                                  :typ   typ}))

    (contains? (free-type-vars typ) t-var)
    (fail "Cannot bind type var. Occurs check." {:t-var t-var
                                                 :typ   typ})

    (and (satisfies? HasKind typ)
         (not= (kind t-var) (kind typ)))
    (fail "Cannot bind type var. Different kinds." {:t-var t-var
                                                    :typ   typ})

    :else
    {t-var typ}))

(defn mgu-fail
  [left right]
  (fail "No MGU." {:left left :right right}))

(defprotocol Unifiable
  (mgu [this other]
    "Find the most general unifier for `this` type and `other` type or return a `fail` if no unifier exists.
     The MGU is a substitution that can produces equal types when applied to `this` and `other`."))

(defn with-unifier
  "Finds the MGU of `type1` and `type2` after applying `substitutions` and composes it with the existing substitutions."
  [substitutions type1 type2]
  (let [unifier (mgu (substitute type1 substitutions)
                     (substitute type2 substitutions))]
    (if (fail? unifier)
      unifier
      (compose-subs unifier substitutions))))

(extend-protocol Unifiable
  TypeVar
  (mgu [this other]
    (bind-var this other))

  TypeConstructor
  (mgu [this other]
    (if (instance? TypeVar other)
      (bind-var other this)
      (if (= this other)
        {}
        (mgu-fail this other))))

  TypeApp
  (mgu [this other]
    (if (instance? TypeVar other)
      (bind-var other this)
      (if (instance? TypeApp other)
        (let [substitutions (mgu (:con this) (:con other))]
          (if (fail? substitutions)
            substitutions
            (if (not= (count (:args this))
                      (count (:args other)))
              (mgu-fail this other)
              (->> (map vector (:args this) (:args other))
                   (reduce (fn [substitutions' [this-arg other-arg]]
                             (with-unifier substitutions' this-arg other-arg))
                           substitutions)))))
        (mgu-fail this other)))))

(defn unifiable?
  "Returns true if the two types can be unified. False otherwise."
  [this-type other-type]
  (not (fail? (mgu this-type other-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme

(defn quantify
  "Creates a Scheme by replacing the free TypeVars in `t-vars` with GenVars of the same kind."
  [t-vars typ]
  (let [t-vars (->> (free-type-vars typ)
                    (sort-by :sym)
                    (filterv #(contains? t-vars %)))
        kinds (mapv :kind t-vars)
        g-vars (mapv #(->GenVar %) (range (count kinds)))
        substitutions (zipmap t-vars g-vars)]
    (->Scheme kinds (substitute typ substitutions))))

(defn generalize
  "Creates a Scheme by replacing all free TypeVars with GenVars of the same kind."
  [typ type-env]
  (quantify (difference (free-type-vars typ)
                        (type-env-free-type-vars type-env))
            typ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Inference Results

(defrecord InferredType [typ substitutions])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sugar

(defn- desugar-scheme
  "Replaces all placeholder ints in the `typ` with a GenVar."
  [typ]
  (postwalk (fn [x]
              (if (int? x) (->GenVar x) x))
            typ))

(defn fn-scheme
  "Syntax sugar for declaring a Scheme over a function type. Useful when declaring the global type-env (aka prelude)."
  [{:keys [kinds args ret]}]
  (desugar-scheme (->Scheme (or kinds [])
                            (fn-type args ret))))

(defn type-ctors
  "Returns a set of all type constructors used in the given type."
  [typ]
  ;; @TODO this might be buggy because the result of `conj!` is not used, which risks losing updates when the transient cannot be updated in place (e.g. tree rebalancing).
  ;; No issues observed so far.
  (let [result (transient #{})]
    (postwalk (fn [x]
                (when (instance? TypeConstructor x)
                  (conj! result x))
                x)
              typ)
    (persistent! result)))