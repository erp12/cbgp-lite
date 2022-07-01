(ns erp12.cbgp-lite.lang.compile
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as w]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.schema-inference.ast :as ast]
            [erp12.schema-inference.inference :as inf]
            [erp12.schema-inference.schema :as sch])
  (:import (clojure.lang Compiler$CompilerException)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State Manipulation

(defn box-ast
  "Wrap the AST in a map with a field for the inferred type."
  [ast env]
  {:ast  ast
   :type (inf/infer-schema env ast)})

(def empty-state
  {:asts (list)
   :push []
   :vars []})

(defn push-ast
  "Push the `ast` to the AST stack in the `state`."
  [ast state]
  (update state :asts #(conj % ast)))

(defn nth-var
  "Get the nth variable from the state using modulo to ensure `n` always selects a
  variable unless no variables are bound in the state. If there are no variables,
  returns nil."
  [n state]
  (let [all-vars (get state :vars)]
    (if (empty? all-vars)
      nil
      (nth all-vars (mod n (count all-vars))))))

(defn macro?
  [ast]
  (and (vector? ast)
       (= (first ast) :var)
       (contains? lib/macros (second ast))))

(defn pop-ast
  "Get the top AST from the ast stack of `state`.

  Returns a map of 2 elements:
    :ast - The popped AST or `:none` if no AST is found.
    :state - The state without the popped AST. If no AST found, the state is unchanged."
  ([state]
   (pop-ast state {}))
  ([state {:keys [allow-macros] :or {allow-macros false}}]
   (loop [remaining (:asts state)
          acc []]
     (let [ast (first remaining)]
       (cond
         (empty? remaining)
         {:ast   :none
          :state state}

         (or (not (macro? (:ast ast))) allow-macros)
         {:ast   ast
          :state (assoc state :asts (concat acc (rest remaining)))}

         :else
         (recur (rest remaining)
                (conj acc ast)))))))

(defn pop-unifiable-ast
  "Get the first AST (from the top) that is unifiable with the given schema.

  Returns a map of 3 elements:
    :ast - The popped AST or `:none` if no AST is found.
    :state - The state without the popped AST. If no AST found, the state is unchanged.
    :bindings - A map of type substitutions used to unify the types."
  ([unify-with state]
   (pop-unifiable-ast unify-with state {}))
  ([unify-with state {:keys [allow-macros] :or {allow-macros false}}]
   (loop [remaining (:asts state)
          acc []]
     (if (empty? remaining)
       {:ast      :none
        :state    state
        :bindings {}}
       (let [ast (first remaining)
             subs (sch/safe-mgu unify-with (:type ast))]
         (if (and subs
                  (or allow-macros
                      (not (macro? (:ast ast)))))
           {:ast      ast
            :state    (assoc state :asts (concat acc (rest remaining)))
            :bindings subs}
           (recur (rest remaining)
                  (conj acc ast))))))))

(defn pop-function-ast
  "Pops the top function AST regardless of argument/return types.
  See `pop-ast` for return structure."
  [state]
  (loop [remaining (:asts state)
         acc []]
    (if (empty? remaining)
      {:ast   :none
       :state state}
      (let [ast (first remaining)]
        (if (sch/fn-schema? (:type ast))
          {:ast   ast
           :state (assoc state :asts (concat acc (rest remaining)))}
          (recur (rest remaining)
                 (conj acc ast)))))))

(defn pop-push-unit
  [state]
  {:push-unit (first (:push state))
   :state     (update state :push rest)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Push to AST Compilation

;(defn ->pprint
;  [x]
;  (println)
;  (clojure.pprint/pprint x)
;  x)

;; @todo Break this into multiple private functions to make testing easier.
(defn push->ast
  [{:keys [push bound-vars ret-type type-env]}]
  (loop [state (assoc empty-state
                 :push push
                 :vars bound-vars)]
    (if (empty? (:push state))
      (let [ast (:ast (pop-unifiable-ast ret-type state {:allow-macros false}))]
        ;(println "EMIT:" ast)
        ast)
      (let [{:keys [push-unit state]} (pop-push-unit state)]
        ;; Start Debug
        ;(println)
        ;(println push-unit)
        ;(doseq [x state] (apply println x))
        ;; End Debug
        (recur
          (match push-unit
            ;; Literals are pushed directly to the AST stack.
            [:lit _]
            (push-ast (box-ast push-unit type-env) state)

            ;; Variable numbers are mapped to a symbol using modulo logic and
            ;; then pushed to the AST stack.
            [:var (_ :guard symbol?)]
            (push-ast (box-ast push-unit type-env) state)

            ;; Variable numbers are mapped to a symbol using modulo logic and
            ;; then pushed to the AST stack.
            [:var (n :guard int?)]
            (push-ast (box-ast (nth-var n state) type-env) state)

            ;; Function applications search for the first AST that returns a function.
            ;; If none found, noop.
            ;; If found, proceed to search for ASTs for each argument to the function.
            ;; If one or more arguments have :t-var types, incrementally bind them.
            (_ :guard #(= :apply %))
            (let [{boxed-ast :ast state-fn-popped :state} (pop-function-ast state)]
              (if (= :none boxed-ast)
                state
                (let [{:keys [ast type]} boxed-ast]
                  (loop [remaining-arg-types (sch/fn-arg-schemas type)
                         bindings {}
                         args []
                         new-state state-fn-popped]
                    (if (empty? remaining-arg-types)
                      ;; Push an AST which calls the function to the arguments and
                      ;; box the AST with the return type of the function.
                      (push-ast {:ast  (vec (concat [:apply ast] (map :ast args)))
                                 :type (sch/substitute-types bindings (sch/fn-ret-schemas type))}
                                new-state)
                      (let [arg-type (first remaining-arg-types)
                            ;; If arg-type is a t-var that we have seen before,
                            ;; bind it to the actual same type as before.
                            arg-type (sch/substitute-types bindings arg-type)
                            is-s-var (sch/s-var? arg-type)
                            ;; If arg-type is still a t-var, pop an ast of any type.
                            ;; Otherwise, pop the AST of the expected type.
                            {arg :ast state-arg-popped :state new-bindings :bindings}
                            (if is-s-var
                              (pop-ast new-state)
                              (pop-unifiable-ast arg-type new-state))
                            ;; If arg-type's type is an unbound s-var, bind the
                            ;; s-var to the type of the popped AST.
                            new-bindings (if is-s-var
                                           {(second arg-type) (:type arg)}
                                           new-bindings)]
                        (if (= :none arg)
                          state
                          (recur (rest remaining-arg-types)
                                 ;; If arg-type is has unbound t-vars that were bound during unification,
                                 ;; add them to the set of bindings.
                                 (sch/compose-substitutions bindings new-bindings)
                                 (conj args arg)
                                 state-arg-popped))))))))

            ;; Nullary function abstraction.
            [:fn]
            (let [{ast :ast new-state :state} (pop-ast state)]
              (if (= :none ast)
                state
                (push-ast {:ast  [:fn [:cat] (:ast ast)]
                           :type [:=> [:cat] (:type ast)]}
                          new-state)))

            ;; Function abstraction with at least 1 argument.
            ;; Compiles a chunk into the body of the function where args can be referenced.
            [:fn & arg-types]
            (let [;; Generate a unique symbol for each argument.
                  arg-vars (repeatedly (count arg-types) #(gensym "a-"))
                  arg-var-asts (map #(vector :var %) arg-vars)
                  ;; Compile a chunk where the arguments are "in-scope" and can appear in ASTs.
                  ;; This is the function's body.
                  fn-body-env (vec (concat type-env (map #(vector := %1 %2) arg-vars arg-types)))
                  body (push->ast {:push       (first (:push state))
                                   :bound-vars (vec (concat bound-vars arg-var-asts))
                                   :ret-type   [:s-var (sch/gen-s-var)]
                                   :type-env   fn-body-env})
                  state-no-body-chunk (update state :push rest)]
              (if (= :none body)
                ;; @todo Should we leave the chunk on the push stack and unpack it?
                state-no-body-chunk
                (push-ast {:ast  [:fn (vec (cons :cat arg-vars)) (:ast body)]
                           :type [:=> (vec (cons :cat arg-types)) (:type body)]}
                          state-no-body-chunk)))

            ;; Searches for an AST to define the local variable.
            ;; Compiles a chunk into the body of the let.
            ;; @todo Should there be N local variables?
            (_ :guard #(= :let %))
            (let [{var-def :ast new-state :state} (pop-ast state)
                  ;; Still pop the "chunk" that is next on the stack.
                  ;; @todo Should we leave the chunk on the push stack and unpack it?
                  noop-state (update state :push rest)]
              (if (= :none var-def)
                noop-state
                (let [;; Generate a unique symbol for the new variable.
                      local-var-symb (gensym "v-")
                      local-var [:var local-var-symb]
                      ;; Compile a chunk where the local variable is "in-scope" and can appear in ASTs.
                      body (push->ast {:push       (first (:push new-state))
                                       :bound-vars (conj bound-vars local-var)
                                       :ret-type   [:s-var (sch/gen-s-var)]
                                       :type-env   (conj type-env [:= local-var-symb (:type var-def)])})]
                  (if (= :none body)
                    noop-state
                    ;; Compose the new `let` AST from the local variable symbol, def, and body.
                    ;; Push the new AST to the state.
                    (push-ast {:ast  [:let [local-var (:ast var-def)] (:ast body)]
                               :type (:type body)}
                              (update new-state :push rest))))))))))))

(defn push->clj
  "Translates Push code into a Clojure form that returns value of type `ret-type`."
  [{:keys [push inputs ret-type type-env dealiases]
    :or   {dealiases {}}}]
  (let [input-vars (vec (map (fn [in] [:var in]) inputs))
        ast (:ast (push->ast {:push       push
                              :bound-vars input-vars
                              :ret-type   ret-type
                              :type-env   type-env}))]
    (->> ast ast/ast->form (w/postwalk-replace dealiases))))

(defn synth-fn
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
