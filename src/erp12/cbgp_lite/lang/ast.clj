(ns erp12.cbgp-lite.lang.ast
  (:import (clojure.lang Compiler$CompilerException)))

(defmulti ast->form (fn [{:keys [op]}] op))

(defmethod ast->form :binding
  [{:keys [name init]}]
  (if (nil? init)
    (symbol name)
    [(symbol name) (ast->form init)]))

(defmethod ast->form :const [{:keys [val]}] val)

(defmethod ast->form :var [{:keys [var]}] (symbol var))

(defmethod ast->form :local [{:keys [name]}] (symbol name))

(defmethod ast->form :invoke
  [{:keys [fn args]}]
  (cons (ast->form fn) (map ast->form args)))

(defmethod ast->form :fn
  [{:keys [methods]}]
  (let [{:keys [params body]} (first methods)]
    (list 'fn
          (mapv ast->form params)
          (ast->form body))))

(defmethod ast->form :fn-method [_]
  (throw (ex-info "unreachable" {})))

(defmethod ast->form :let
  [{:keys [bindings body]}]
  (let []
    (list 'let
          (vec (mapcat ast->form bindings))
          (ast->form body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ast-size (fn [{:keys [op]}] op))

(defmethod ast-size :const [_] 1)
(defmethod ast-size :var [_] 1)
(defmethod ast-size :local [_] 1)

(defmethod ast-size :invoke
  [{:keys [fn args]}]
  (reduce (clojure.core/fn [i arg] (+ i (ast-size arg)))
          (ast-size fn)
          args))

(defmethod ast-size :fn
  [{:keys [methods]}]
  (->> methods
       (map ast-size)
       (reduce +)
       inc))

(defmethod ast-size :fn-method
  [{:keys [params body]}]
  (+ (count params)
     (ast-size body)))

(defmethod ast-size :let
  [{:keys [bindings body]}]
  (reduce (fn [i binding] (+ i (ast-size binding)))
          (inc (ast-size body))
          bindings))

(defmethod ast-size :binding
  [{:keys [init]}]
  (if (nil? init) 0 (ast-size init)))
