(ns erp12.cbgp-lite.program.expr
  "Records and functions for constructing expressions ASTs, inferring expression types, and exploring expression structures."
  (:require [clojure.walk :refer [postwalk]]
            [erp12.cbgp-lite.program.lib :refer [guard]]
            [erp12.cbgp-lite.program.types :as t])
  (:import (clojure.lang Compiler$CompilerException)))

;; A variable expression.
(defrecord Var [sym])

;; A typed literal expression.
(defrecord Lit [val typ])

;; A function application expression.
;; `func` is an expression returning a function type.
;; `args` is a vector of argument expressions.
(defrecord App [func args])

;; A function abstraction (aka lambda) expression.
;; `params` is a vector of symbols for the function's parameters.
;; `body` is an expression where params might be referenced.
(defrecord Abs [params body])

;; A let binding expression
;; `bindings` is a vector of local variable binding maps, each containing:
;;    `:sym` the symbol of the local variable
;;    `:def` an expression for the definition of the local variable.
;; Subsequent bindings may reference local vars created in previous bindings within the same Let.
(defrecord Let [bindings body])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infer Type of Expressions

(defprotocol InferType
  (ti [this type-env substitutions] "Infer type of expression."))

(extend-protocol InferType
  Var
  (ti [this type-env substitutions]
    (let [scheme (get type-env (:sym this))]
      (if (nil? scheme)
        (t/fail "Unbound variable." this)
        (t/->InferredType (t/instantiate-scheme scheme)
                          substitutions))))

  Lit
  (ti [this type-env substitutions]
    (t/->InferredType (:typ this) substitutions))

  App
  (ti [this type-env substitutions]
    (let [fn-ti (ti (:func this) type-env substitutions)]
      (if (t/fail? fn-ti)
        fn-ti
        (let [arg-tis (loop [remaining-args (:args this)
                             substitutions  (:substitutions fn-ti)
                             arg-tis        []]
                        (if (empty? remaining-args)
                          arg-tis
                          (let [arg    (first remaining-args)
                                arg-ti (ti arg type-env substitutions)]
                            (if (t/fail? arg-ti)
                              arg-ti
                              (recur (rest remaining-args)
                                     (:substitutions arg-ti)
                                     (conj arg-tis arg-ti))))))]
          (if (t/fail? arg-tis)
            arg-tis
            (let [substitutions (:substitutions (last arg-tis))
                  t-var         (t/->TypeVar (gensym "t-") :*)
                  substitutions (t/with-unifier substitutions
                                  (t/fn-type (mapv :typ arg-tis) t-var)
                                  (:typ fn-ti))]
              (if (t/fail? substitutions)
                substitutions
                (t/->InferredType t-var substitutions))))))))

  Abs
  (ti [this type-env substitutions]
    (let [t-vars   (repeatedly (count (:params this))
                               #(t/->TypeVar (gensym "t-") :*))
          schemes  (map #(t/->Scheme [] %) t-vars)
          type-env (into type-env (zipmap (:params this) schemes))
          body-ti  (ti (:body this) type-env substitutions)]
      (if (t/fail? body-ti)
        body-ti
        (t/->InferredType (t/fn-type (vec t-vars) (:typ body-ti))
                          (:substitutions body-ti)))))

  Let
  (ti [this type-env substitutions]
    (loop [remaining     (:bindings this)
           type-env      type-env
           substitutions substitutions]
      (if (empty? remaining)
        (let [body-ti (ti (:body this) type-env substitutions)]
          (if (t/fail? body-ti)
            body-ti
            (t/->InferredType (:typ body-ti) (:substitutions body-ti))))
        (let [binding (first remaining)
              def-ti  (ti (:def binding) type-env substitutions)]
          (if (t/fail? def-ti)
            def-ti
            (let [;; Make sure :sym is removed from type-env before calling generalize
                  type-env (dissoc type-env (:sym binding))
                  scheme   (t/generalize (t/substitute (:typ def-ti) (:substitutions def-ti))
                                         type-env)]
              (recur (rest remaining)
                     (assoc type-env (:sym binding) scheme)
                     (:substitutions def-ti)))))))))

(defn infer-type
  "Run the type inference algorithm and render a human-friendly version of the type by applying all substitutions."
  [expr type-env]
  (let [result (ti expr type-env {})]
    (if (t/fail? result)
      result
      (t/substitute (:typ result) (:substitutions result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression AST to Clojure Forms

(defmacro guarded-fn
  [params body]
  (list 'fn 'abs-fn params `(guard ~body)))

(defprotocol Formable
  (to-form [this] "Return AST as a Clojure form"))

(extend-protocol Formable
  Var
  (to-form [this] (:sym this))

  Lit
  (to-form [this] (:val this))

  App
  (to-form [this]
    (cons (to-form (:func this))
          (map to-form (:args this))))

  Abs
  (to-form [this]
    (list `guarded-fn
          (vec (:params this))
          (to-form (:body this))))

  Let
  (to-form [this]
    (list 'let
          (vec (mapcat (fn [binding]
                         [(:sym binding) (to-form (:def binding))])
                       (:bindings this)))
          (to-form (:body this))))

  clojure.lang.PersistentArrayMap
  (to-form [this]
    (throw (ex-info "to-form PAM" {:value this}))))

(defn form->fn
  "Given a vector of argument symbols and a Clojure form (`body`)
                             create a function (similar to `fn`)."
  [args body]
  (try
    (eval `(fn ~(vec args) ~body))
    (catch Compiler$CompilerException e
      (throw (ex-info (str "Bad form: " body)
                      {:args args
                       :body body}
                      e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Measuring the size and shape of expressions

(defprotocol Expr
  (expr-size [this] "The number of nodes in the expression AST")
  (expr-depth' [this depth] "The max depth of the expression AST. Internal implementation. See expr-depth.")
  (de-bruijn' [this env]))

(extend-protocol Expr
  Var
  (expr-size [this] 1)
  (expr-depth' [this depth] (inc depth))
  (de-bruijn' [this env]
    (let [idx (first (keep-indexed #(when (= (:sym this) %2) %1) env))]
      {:kind :Var
       :node (if idx
               {:de-bruijn idx}
               (:sym this))}))

  Lit
  (expr-size [this] 1)
  (expr-depth' [this depth] (inc depth))
  (de-bruijn' [this env] {:kind :Lit
                          :node (:val this)})

  App
  (expr-size [this]
    (reduce (fn [n arg] (+ n (expr-size arg)))
            ;; One larger than function size to include the App node.
            (inc (expr-size (:func this)))
            (:args this)))
  (expr-depth' [this depth]
    (->> (:args this)
         (map #(expr-depth' % (inc depth)))
         (reduce max)
         (max (expr-depth' (:func this) (inc depth)))))
  (de-bruijn' [this env]
    {:kind     :App
     :children (mapv #(de-bruijn' % env) (cons (:func this) (:args this)))})

  Abs
  (expr-size [this]
    (+ (count (:params this))
       (expr-size (:body this))
       ;; One more to include the Abs node.
       1))
  (expr-depth' [this depth]
    (expr-depth' (:body this) (inc depth)))
  (de-bruijn' [this env]
    {:kind     :Abs
     :children [(de-bruijn' (:body this)
                            (concat (reverse (:params this))
                                    env))]})

  Let
  (expr-size [this]
    (reduce (fn [n binding]
              ;; + 1 for the var
              (+ n 1 (expr-size (:def binding))))
              ;; One larger than the body to include the Let node.
            (inc (expr-size (:body this)))
            (:bindings this)))
  (expr-depth' [this depth]
    (->> (:bindings this)
         (map #(expr-depth' (:def %) (inc depth)))
         (reduce max)
         (max (expr-depth' (:body this) (inc depth)))))
  (de-bruijn' [this env]
    (loop [[binding & remaining-bindings] (:bindings this)
           normalized-bindings            []
           env'                           env]
      (if binding
        (recur remaining-bindings
               (conj normalized-bindings (de-bruijn' (:def binding) env'))
               (cons (:sym binding) env'))
        {:kind      :Let
         :children (conj normalized-bindings
                         (de-bruijn' (:body this) env'))}))))

(defn expr-depth
  "The max depth of the expression AST."
  [expr]
  (expr-depth' expr 0))

(defn de-bruijn
  [expr]
  (de-bruijn' expr (list)))

(defn special-var?
  "True if the expr is a Var denoting one of Clojure's special symbols."
  [expr]
  (and (instance? Var expr)
       (special-symbol? (:sym expr))))

(defn occurs?
  "True if the `term` exists in the `ast` (expression or type) and false otherwise."
  [term ast]
  (let [t (transient #{})]
    (postwalk (fn [x]
                #_{:clj-kondo/ignore [:unused-value]}
                (conj! t (= x term))
                x)
              ast)
    (contains? t true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Form Edit Distance

(defn- leftmost-root
  [tree-or-forest]
  (if (vector? tree-or-forest)
    ;; Forest
    (first tree-or-forest)
    ;; Tree
    tree-or-forest))

(defn- remove-leftmost-root
  [tree-or-forest]
  (if (vector? tree-or-forest)
    ;; Forest
    (concat (:children (first tree-or-forest)) (rest tree-or-forest))
    ;; Tree
    (:children tree-or-forest)))

(defn- remove-leftmost-subtree
  [tree-or-forest]
  (if (vector? tree-or-forest)
    ;; Forest
   (vec (rest tree-or-forest))
    ;; Tree
   nil))

(defn- forest-size
  [tree-or-forest]
  (cond (vector? tree-or-forest)
        (reduce + (map forest-size tree-or-forest))

        (:children tree-or-forest)
        (inc (forest-size (:children tree-or-forest)))
        
        :else 1))

(defn- insert-delete-cost
  [node]
  (forest-size node))

(defn- rename-cost
  [from to]
  (if (= (:kind from) (:kind to))
    (if (= (:node from) (:node to))
      0
      1)
    ;; "Renaming" totally different subtrees is the cost to delete and insert everything 
    (+ (insert-delete-cost from)
       (insert-delete-cost to))))

(defn tree-edit-distance
  [t1 t2]
  (cond (and (empty? t1) (empty? t2))
        0

        (empty? t1)
        (+ (tree-edit-distance t1 (remove-leftmost-root t2))
           (insert-delete-cost (leftmost-root t2)))

        (empty? t2)
        (+ (tree-edit-distance (remove-leftmost-root t1) t2)
           (insert-delete-cost (leftmost-root t1)))

        :else
        (min (+ (tree-edit-distance t1 (remove-leftmost-root t2))
                (insert-delete-cost (leftmost-root t2)))
             (+ (tree-edit-distance (remove-leftmost-root t1) t2)
                (insert-delete-cost (leftmost-root t1)))
             (+ (tree-edit-distance (:children (leftmost-root t1)) (:children (leftmost-root t2)))
                (tree-edit-distance (remove-leftmost-subtree t1)
                                    (remove-leftmost-subtree t2))
                (rename-cost (leftmost-root t1) (leftmost-root t2))))))
