(ns erp12.cbgp-lite.lang.compile
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [erp12.cbgp-lite.lang.ast :as a]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.schema :as schema]
            [taoensso.timbre :as log]))

(def collect-types? (atom false))
(def types-seen (atom {}))

;; @todo Move to schema-inference
(defn tap-nodes
  [f tree]
  (w/walk (partial tap-nodes f) identity (f tree)))

;; @todo Move to schema-inference
(defn s-vars
  [schema]
  (let [x (transient #{})]
    (tap-nodes
      (fn [node]
        (when (= (:type node) :s-var)
          (conj! x (:sym node)))
        node)
      schema)
    (persistent! x)))

(defn canonical-type
  [type]
  (let [subs (into {} (map-indexed (fn [i s] [s (symbol (str "S" i))])
                                   (sort (s-vars type))))]
    (w/postwalk-replace subs type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stack AST sizes

(def sketches (atom {}))

(defn record-asts!
  [state]
  (let [sketch (->> state
                    :asts
                    (map (fn [{::keys [ast type]}]
                           {:root (:op ast)
                            :size (a/ast-size ast)
                            :type type})))]
    (swap! sketches
           #(assoc % (->> state
                          :asts
                          (map (fn [{::keys [ast type]}]
                                 {:root (:op ast)
                                  :size (a/ast-size ast)
                                  :type type})))
                     (inc (or (get % sketch) 0))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applications

(def apply-events
  (atom {:success 0
         :no-fn   0
         :no-arg  0}))

(defn apply-success! []
  (swap! apply-events update :success inc))

(defn apply-no-fn! []
  (swap! apply-events update :no-fn inc))

(defn apply-no-arg! []
  (swap! apply-events update :no-arg inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State Manipulation

(def empty-state
  {:asts    (list)
   :push    []
   :locals  []
   ;; @todo Experimental
   :biggest :none
   :newest  :none})

(defn macro?
  [{:keys [op] :as ast}]
  (let [sym (case op
              :var (:var ast)
              :local (:name ast)
              :VAR (:sym ast)
              nil)]
    (if sym
      (contains? lib/macros sym)
      false)))

(defn unifiable?
  [unify-with typ]
  (not (schema/mgu-failure? (schema/mgu unify-with typ))))

(defn push-ast
  "Push the `ast` to the AST stack in the `state`."
  [ast {:keys [biggest newest ret-type] :as state}]
  (when @collect-types?
    (swap! types-seen
           (fn [m t] (assoc m t (inc (get m t 0))))
           (canonical-type (::type ast))))
  (let [output-able? (and (unifiable? ret-type (::type ast))
                          (not (macro? (::ast ast))))
        newest-out-ast (if output-able? ast newest)
        biggest-out-ast (if (and output-able?
                                 (or (= biggest :none)
                                     (> (a/ast-size (::ast ast))
                                        (a/ast-size (::ast biggest)))))
                          ast
                          biggest)]
    (assoc state
      :asts (conj (:asts state) ast)
      :biggest biggest-out-ast
      :newest newest-out-ast)))

(defn nth-local
  "Get the nth variable from the state using modulo to ensure `n` always selects a
  variable unless no variables are bound in the state. If there are no variables,
  returns nil."
  [n state]
  (let [locals (get state :locals)]
    (if (empty? locals)
      nil
      (nth locals (mod n (count locals))))))

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

         (or (not (macro? (::ast ast))) allow-macros)
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
             subs (schema/mgu unify-with (::type ast))]
         (if (and (not (schema/mgu-failure? subs))
                  (or allow-macros
                      (not (macro? (::ast ast)))))
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
      (let [ast (first remaining)
            schema-type (get-in ast [::type :type])
            schema-type (if (= schema-type :scheme)
                          (get-in ast [::type :body :type])
                          schema-type)]
        (if (= schema-type :=>)
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

(declare push->ast)

(defmulti compile-step (fn [{:keys [push-unit]}] (:gene push-unit)))

(defmethod compile-step :lit
  [{:keys [push-unit state]}]
  ;; Literals are pushed directly to the AST stack.
  ;; Type annotation is taken from push-unit.
  (let [{:keys [val type]} push-unit]
    (push-ast {::ast  {:op :const :val val}
               ::type type}
              state)))

(defmethod compile-step :var
  [{:keys [push-unit state type-env]}]
  ;; Vars are pushed directly to the AST stack.
  ;; Type is taken from the type environment.
  (push-ast {::ast  {:op :var :var (symbol (:name push-unit))}
             ::type (schema/instantiate (get type-env (:name push-unit)))}
            state))

(defmethod compile-step :local
  [{:keys [push-unit state type-env]}]
  ;; Local variable numbers are mapped to a symbol using modulo logic and
  ;; then pushed to the AST stack.
  (let [local-symbol (nth-local (:idx push-unit) state)]
    (if (nil? local-symbol)
      state
      (push-ast {::ast  {:op :local :name local-symbol}
                 ::type (schema/instantiate (get type-env local-symbol))}
                state))))

(defmethod compile-step :apply
  [{:keys [state type-env]}]
  ;; Function applications search for the first AST that returns a function.
  ;; If none found, return state.
  ;; If found, proceed to search for ASTs for each argument to the function.
  ;; If one or more arguments have :s-var types, incrementally bind them.
  (let [{boxed-ast :ast state-fn-popped :state} (pop-function-ast state)]
    (if (= :none boxed-ast)
      (do (apply-no-fn!)
          state)
      (let [{::keys [ast type]} boxed-ast]
        (loop [remaining-arg-types (schema/fn-arg-schemas type)
               bindings {}
               args []
               new-state state-fn-popped]
          (if (empty? remaining-arg-types)
            ;; Push an AST which calls the function to the arguments and
            ;; box the AST with the return type of the function.
            (do (apply-success!)
                (push-ast {::ast  {:op   :invoke
                                   :fn   ast
                                   :args (mapv ::ast args)}
                           ::type (schema/instantiate (schema/substitute bindings (schema/fn-ret-schema type)))}
                          new-state))
            (let [arg-type (first remaining-arg-types)
                  ;; If arg-type is a t-var that we have seen before,
                  ;; bind it to the actual same type as before.
                  arg-type (schema/substitute bindings arg-type)
                  is-s-var (= (:type arg-type) :s-var)
                  ;; If arg-type is still a t-var, pop an ast of any type.
                  ;; Otherwise, pop the AST of the expected type.
                  {arg :ast state-arg-popped :state new-bindings :bindings}
                  (if is-s-var
                    (pop-ast new-state)
                    (pop-unifiable-ast arg-type new-state))
                  ;; If arg-type's type is an unbound s-var, bind the
                  ;; s-var to the type of the popped AST.
                  new-bindings (if (and is-s-var (not= arg :none))
                                 {;; The symbol of the s-var
                                  (:sym arg-type)
                                  ;; The type of the var made as concrete as possible.
                                  (schema/generalize type-env (schema/substitute new-bindings (::type arg)))}
                                 new-bindings)]
              (if (= :none arg)
                (do (apply-no-arg!)
                    state)
                (recur (rest remaining-arg-types)
                       ;; If arg-type is has unbound t-vars that were bound during unification,
                       ;; add them to the set of bindings.
                       (schema/compose-substitutions bindings new-bindings)
                       (conj args arg)
                       state-arg-popped)))))))))

(defmethod compile-step :fn
  [{:keys [push-unit state type-env]}]
  (let [arg-types (:arg-types push-unit)]
    (if (empty? arg-types)
      ;; Compile nullary function.
      (let [{ast :ast new-state :state} (pop-ast state)]
        (if (= :none ast)
          state
          (push-ast {::ast  {:op      :fn
                             :methods [{:op     :fn-method
                                        :params []
                                        :body   (::ast ast)}]}
                     ::type {:type   :=>
                             :input  {:type :cat :children []}
                             :output (schema/instantiate (::type ast))}}
                    new-state)))
      ;; Compile n-ary function
      (let [;; Generate a unique symbol for each argument.
            arg-vars (repeatedly (count arg-types) #(gensym "a-"))
            ;; Compile a chunk where the arguments are "in-scope" and can appear in ASTs.
            ;; This is the function's body.
            fn-body-env (into type-env (map #(vector %1 %2) arg-vars arg-types))
            body (push->ast {:push     (first (:push state))
                             :locals   (vec (concat (:locals state) arg-vars))
                             :ret-type {:type :s-var :sym (gensym "S")}
                             :type-env fn-body-env})
            state-no-body-chunk (update state :push rest)
            ;; Filter out unused args
            args+types (mapcat (fn [a t]
                                 (if (schema/occurs? a body)
                                   [[a t]]
                                   []))
                               arg-vars
                               arg-types)]
        (if (= :none body)
          ;; @todo Should we leave the chunk on the push stack and unpack it?
          state-no-body-chunk
          (push-ast {::ast  {:op      :fn
                             :methods [{:op     :fn-method
                                        :params (mapv (fn [[a _]] {:op :binding :name a})
                                                      args+types)
                                        :body   (::ast body)}]}
                     ::type {:type   :=>
                             :input  {:type     :cat
                                      :children (mapv second args+types)}
                             :output (schema/instantiate (::type body))}}
                    state-no-body-chunk))))))

(defmethod compile-step :let
  [{:keys [state type-env]}]
  (let [{var-def :ast new-state :state} (pop-ast state)
        ;; Still pop the "chunk" that is next on the stack.
        ;; @todo Should we leave the chunk on the push stack and unpack it?
        noop-state (update state :push rest)]
    (if (= :none var-def)
      noop-state
      (let [;; Generate a unique symbol for the new variable.
            local-var-symb (gensym "v-")
            ;; Compile a chunk where the local variable is "in-scope" and can appear in ASTs.
            body (push->ast {:push     (first (:push new-state))
                             :locals   (vec (conj (:locals state) local-var-symb))
                             :ret-type {:type :s-var :sym (gensym "S")}
                             :type-env (conj type-env [local-var-symb (::type var-def)])})]
        (if (= :none body)
          noop-state
          ;; Compose the new `let` AST from the local variable symbol, def, and body.
          ;; Push the new AST to the state.
          (push-ast {::ast  {:op       :let
                             :bindings [{:op   :binding
                                         :name local-var-symb
                                         :init (::ast var-def)}]
                             :body     (::ast body)}
                     ::type (::type body)}
                    (update new-state :push rest)))))))

(defn default-state-output-fn
  [{:keys [ret-type] :as state}]
  (-> ret-type
      schema/instantiate
      (pop-unifiable-ast state {:allow-macros false})
      :ast))

(defn- state->log
  [state]
  (str "\n" (str/join "\n" (map #(apply pr-str %) state))))

(defn push->ast
  [{:keys [push locals ret-type type-env dealiases state-output-fn record-sketch?]
    :or   {dealiases      lib/dealiases
           record-sketch? false}}]
  (let [state-output-fn (or state-output-fn default-state-output-fn)]
    (loop [state (assoc empty-state
                   ;; Ensure a list
                   :push (reverse (into '() push))
                   :locals locals
                   :ret-type ret-type)]
      (if (empty? (:push state))
        (let [_ (log/trace "Final:" (state->log state))
              ;; @todo Experimental - record final stack AST sizes and types.
              _ (when record-sketch?
                  (record-asts! state))
              ast (w/postwalk-replace dealiases (state-output-fn state))]
          (log/trace "EMIT:" ast)
          ast)
        (let [{:keys [push-unit state]} (pop-push-unit state)]
          (log/trace "Current:" push-unit (state->log state))
          (recur (compile-step {:push-unit push-unit
                                :type-env  type-env
                                :state     state})))))))
