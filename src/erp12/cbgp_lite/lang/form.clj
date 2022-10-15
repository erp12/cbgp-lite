(ns erp12.cbgp-lite.lang.form
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
