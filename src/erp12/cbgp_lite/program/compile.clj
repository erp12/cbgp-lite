(ns erp12.cbgp-lite.program.compile
  "A namespace for compiling typed ASTs from push code using a stack machine."
  (:require [erp12.cbgp-lite.genome]
            [erp12.cbgp-lite.program.expr :as e]
            [erp12.cbgp-lite.program.types :as t])
  (:import (erp12.cbgp_lite.genome Lit Var Local App Abs Let)))

(defrecord Ast [expr typ])

(def empty-state
  {:asts        (list)
   :push        (list)
   :locals      []})

(defn push-ast
  "Pushes the `ast` to the AST stack in the `state` and triggers the ::push hook."
  ([ast state]
   (push-ast ast state {}))
  ([ast state {hook :push}]
   (let [state' (assoc state :asts (conj (:asts state) ast))]
     (when hook
       (hook ast state state'))
     state')))

(defn pop-ast
  "Get the top AST from the ast stack of `state`.
   Returns a map of 2 elements:
     :ast - The popped AST or `:none` if no AST is found.
     :state - The state without the popped AST. If no AST found, the state is unchanged."
  ([state]
   (pop-ast state {}))
  ([state {hook :pop}]
   (let [result (loop [[ast & more] (:asts state)
                       acc          []]
                  (cond
                    (nil? ast)
                    {:ast   :none
                     :state state}

                    ;; Special vars like 'if and 'do cannot be passed to other functions.
                    (not (e/special-var? (:expr ast)))
                    {:ast   ast
                     :state (assoc state :asts (concat acc more))}

                    :else
                    (recur more
                           (conj acc ast))))]
     (when hook
       (hook (:ast result) state (:state result)))
     result)))

(defn pop-unifiable-ast
  "Get the first AST (from the top) that is unifiable with the given schema.

  Returns a map of 3 elements:
    :ast           - The popped AST or `:none` if no AST is found.
    :state         - The state without the popped AST. If no AST found, the state is unchanged.
    :substitutions - A map of type substitutions used to unify the types."
  ([unify-with state]
   (pop-unifiable-ast unify-with state {}))
  ([unify-with state {hook :pop}]
   (let [result (loop [[ast & more] (:asts state)
                       acc          []]
                  (if ast
                    (let [substitutions (t/mgu unify-with (:typ ast))]
                      (if (or (t/fail? substitutions)
                               ;; Special vars like 'if and 'do cannot be passed to other functions.
                              (e/special-var? (:expr ast)))
                        (recur more
                               (conj acc ast))
                        {:ast           ast
                         :state         (assoc state :asts (concat acc more))
                         :substitutions substitutions}))
                    {:ast           :none
                     :state         state
                     :substitutions {}}))]
     (when hook
       (hook (:ast result) state (:state result)))
     result)))

(defn pop-function-ast
  "Pops the top function AST regardless of argument/return types.
  See `pop-ast` for return structure."
  ([state]
   (pop-function-ast state {}))
  ([state {hook :pop}]
   (let [result (loop [[ast & more] (:asts state)
                       acc          []]
                  (if ast
                    (if (t/fn-type? (:typ ast))
                      {:ast   ast
                       :state (assoc state :asts (concat acc more))}
                      (recur more
                             (conj acc ast)))
                    {:ast   :none
                     :state state}))]
     (when hook
       (hook (:ast result) state (:state result)))
     result)))

(defn nth-local
  "Get the nth variable from the state using modulo to ensure `n` always selects a
  variable unless no variables are bound in the state. If there are no variables,
  returns nil."
  [state n]
  (let [locals (get state :locals)]
    (if (empty? locals)
      nil
      (nth locals (mod n (count locals))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Push to AST Compilation

(declare push->ast)

(defprotocol Compilable
  "A protocol for objects that can be used by the CBGP compiler."
  (compile-step [this state type-env hooks] "Processes this object against the current compiler state."))

(extend-protocol Compilable
  Lit
  (compile-step [this state type-env {hook :lit
                                      :as  hooks}]
    ;; Literal expressions are pushed directly to th stack.
    ;; Type annotations are taken directly from the push unit.
    (let [state' (push-ast (->Ast (e/->Lit (:val this) (:typ this))
                                  (:typ this))
                           state
                           hooks)]
      (when hook
        (hook this state state'))
      state'))

  Var
  (compile-step [this state type-env {hook :var
                                      :as  hooks}]
    ;; Vars are pushed directly to the AST stack.
    ;; Types are taken from the type environment via a lookup.
    (let [scheme (get type-env (:sym this))
          _      (when (nil? scheme)
                   (throw (ex-info "Var not found in type environment." {:var this})))
          state' (push-ast (->Ast (e/->Var (:sym this))
                                  (t/instantiate-scheme scheme))
                           state
                           hooks)]
      (when hook
        (hook this state state'))
      state'))

  Local
  (compile-step [this state type-env {hook :local
                                      :as  hooks}]
    ;; Local variable numbers are mapped to a symbol using modulo logic and
    ;; then pushed to the AST stack.
    ;; @TODO Modulo logic is known to be problematic here.
    (let [local-sym (nth-local state (:idx this))
          scheme    (get type-env local-sym)
          state'    (cond
                      (nil? local-sym)
                      (do (when hook (hook this state :noop))
                          state)

                      (nil? scheme)
                      (throw (ex-info "Local not found in type environment." {:sym local-sym}))

                      :else
                      (push-ast (->Ast (e/->Var local-sym)
                                       (t/instantiate-scheme (get type-env local-sym)))
                                state
                                hooks))]
      (when hook
        (hook this state state'))
      state'))

  App
  (compile-step [this state type-env {hook :app
                                      :as  hooks}]
    ;; Function application pops the top-most AST with a function type
    ;; and a type-safe AST for each function argument. 
    ;; The composition of those ASTs is pushed back to the stack. 
    (let [noop-hook #(when hook (hook this state :noop))
          ;; Get an AST with any function type from the stack.
          {fn-ast      :ast
           state-no-fn :state} (pop-function-ast state hooks)]
      (if (= :none fn-ast)
        ;; Noop if there were no function ASTs on the stack.
        (do (noop-hook)
            state)
        ;; For each argument type of the function ... 
        (loop [[arg-type & more-arg-type] (butlast (:args (:typ fn-ast)))
               substitutions              {}
               arg-asts                   []
               state'                     state-no-fn]
          (if arg-type
            ;; When some args still need to be found ...
            (let [;; Make sure all bound type variables are replaced with their type.
                  arg-type (t/substitute arg-type substitutions)
                  ;; Pop the top-most AST with a compatible type for the next argument.
                  popped   (pop-unifiable-ast arg-type state' hooks)
                  arg-ast  (:ast popped)]
              (if (= :none arg-ast)
                ;; Noop if there are no ASTs to satisfy the argument.
                (do (noop-hook)
                    state)
                ;; Move on to the next argument ...
                (recur more-arg-type
                       (t/compose-subs (:substitutions popped) substitutions)
                       (conj arg-asts arg-ast)
                       (:state popped))))
            ;; When valid ASTs found for all args ...
            (let [;; Create a fresh TypeVar for the return type of this expression.
                  ret-type      (t/fresh-type-var :*)
                  ;; Find the substitutions that unify the function AST's type and 
                  ;; a function type created from the arg ASTs type's and fresh return TypeVar.
                  ;; Compose the new substitutions with the existing substitutions. 
                  substitutions (t/with-unifier substitutions
                                  (:typ fn-ast)
                                  (t/fn-type (mapv :typ arg-asts) ret-type))]
              (if (t/fail? substitutions)
                ;; Unification should never fail if incremental type checking of args was successful.
                (throw (ex-info "Unreachable." {}))
                ;; Push the composite AST to the stack.
                (let [state' (push-ast (->Ast (e/->App (:expr fn-ast)
                                                       (mapv :expr arg-asts))
                                              (t/substitute ret-type substitutions))
                                       state'
                                       hooks)]
                  (when hook
                    (hook this state state'))
                  state'))))))))

  Abs
  (compile-step [this state type-env {hook :abs
                                      :as  hooks}]
    (when hook
      (hook this state))
    ;; Fn genes trigger a nested call to the cbgp compiler to produce a lambda (Abs) expression.
    ;; The new function's param and return types are taken from the gene.
    ;; The push code to compile for the new function's body is also taken from the gene.
    (let [noop-hook #(when hook (hook this state :noop))]
      (if (empty? (:param-types this))
        ;; Compile nullary function
        (let [{ast    :ast
               state' :state} (pop-unifiable-ast (:ret-type this) state hooks)]
          (if (= :none ast)
            (do (noop-hook)
                state)
            (let [state' (push-ast (->Ast (e/->Abs [] (:expr ast))
                                          (t/fn-type [] (:typ ast)))
                                   state'
                                   hooks)]
              (when hook
                (hook this state state'))
              state')))
        ;; Compile n-ary function
        (let [params+types  (mapv vector (:param-symbols this) (:param-types this))
              ;; Create a type environment and locals vector database with params added.
              type-env'     (->> params+types
                                 (mapv (fn [[sym typ]] [sym (t/->Scheme [] typ)]))
                                 (into type-env))
              locals'       (vec (concat (:locals state) (:param-symbols this)))
              ;; Compile a chunk where the arguments are "in-scope" and can appear in ASTs.
              ;; This is the function's body.
              body-ast      (push->ast {:push        (:push this)
                                        :output-type (:ret-type this)
                                        :locals      locals'
                                        :type-env    type-env'
                                        :hooks       hooks})]
          (if (= :none body-ast)
            (do (noop-hook)
                state)
            (let [state' (push-ast (->Ast (e/->Abs (mapv first params+types)
                                                   (:expr body-ast))
                                          (t/fn-type (mapv second params+types)
                                                     (:typ body-ast)))
                                   state
                                   hooks)]
              (when hook
                (hook this state state'))
              state'))))))

  Let
  (compile-step [this state type-env {hook :let
                                      :as  hooks}]
    ;; Let genes use the top AST as the definition for a new local variable.
    ;; A nested call to the cbgp compiler is then used to produce a the body.
    ;; The push code to compile is taken from the gene.
    (let [noop-hook #(when hook (hook this state :noop))
          ;; The AST for the definition of the local var.
          pop-result (pop-ast state hooks)
          def-ast    (:ast pop-result)
          state'     (if (= :none def-ast)
                       (do (noop-hook)
                           state)
                       (let [;; Compile a chunk where the local variable is "in-scope" and can appear in ASTs.
                             body-ast (push->ast {:push        (:push this)
                                                  :output-type (t/->TypeVar (gensym "t-") :*)
                                                  :locals      (vec (conj (:locals state) (:sym this)))
                                                  :type-env    (assoc type-env (:sym this) (t/generalize (:typ def-ast) type-env))
                                                  :hooks       hooks})]
                         (if (= :none body-ast)
                           (do (noop-hook)
                               state)
                           (push-ast (->Ast (e/->Let [{:sym (:sym this)
                                                       :def (:expr def-ast)}]
                                                     (:expr body-ast))
                                            (:typ body-ast))
                                     (:state pop-result)
                                     hooks))))]
      (when hook
        (hook this state state'))
      state')))

(defn push->ast
  [{:keys [push output-type type-env locals hooks]
    :or {locals [] hooks {}}
    :as opts}]
  (when (:start hooks)
    ((:start hooks) opts))
  (loop [state (assoc empty-state
                      ;; Ensure push is a list
                      :push (reverse (into '() push))
                      :locals locals)]
    (when (:step hooks)
      ((:step hooks) state))
    (if (empty? (:push state))
      ;; Done compiling push. Pop the top-most AST compatible with problem's output type.
      (let [result (pop-unifiable-ast output-type state hooks)
            output-ast (:ast result)
            output-ast (if (= :none output-ast)
                         output-ast
                         (assoc output-ast
                                :typ (t/substitute (:typ output-ast) (:substitutions result))))]
        (when (:end hooks)
          ((:end hooks) (:ast result) (:state result)))
        output-ast)
      ;; Compile the next push unit.
      (let [unit (first (:push state))
            state'  (update state :push rest)
            state' (try
                     (compile-step unit state' type-env hooks)
                     (catch Exception e
                       (throw (ex-info "Exception while compiling unit." {:unit unit :state state'} e))))]
        (recur state')))))
