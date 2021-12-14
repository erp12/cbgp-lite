(ns erp12.cbgp-lite.demo
  (:require [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.lang.lib :as lib]))

;; This file contains demonstrations of compiling Push code to Clojure code.
;; Evaluate the forms in a REPL.

(comment
  ;; Simple Expression

  ;; The Push code to "compile" into Clojure.
  (def push
    [[:lit 100]
     [:var 'in1]
     [:var 'int-add]
     :apply])

  ;; Compile the Push to Clojure code.
  (def code
    (c/push->clj {:push          push
                  :inputs        ['in1]
                  :ret-type      int?
                  :type-env      (conj lib/environment [:= 'in1 int?])
                  :dealiases lib/dealiases}))

  code

  ;; A "real" Clojure function defined with code.
  (def plus100
    (c/synth-fn ['in1] code))

  ;; Test the function.
  (doseq [i (range 10)]
    (println i (plus100 i)))

  ;; Format the code in copy-paste safe way.
  `(fn ~(vec ['in1]) ~code)

  )

(comment
  ;; Conditional Logic

  (def push
    [[:lit "large"]
     [:lit "small"]
     [:lit 1000]
     [:var 0]
     [:var 'int-lt]
     :apply
     [:var 'if]
     :apply])

  (def code
    (c/push->clj {:push          push
                  :inputs        ['in1]
                  :ret-type      string?
                  :type-env      (conj lib/environment [:= 'in1 int?])
                  :dealiases lib/dealiases}))

  code

  (def small-or-large
    (c/synth-fn ['in1] code))

  (doseq [i [100 1000 10000]]
    (println i (small-or-large i)))

  `(fn ~(vec ['in1]) ~code)

  )

(comment
  ;; Let bindings

  (def push
    [;; [:var 0] Binds to 'in1
     [:var 0]
     [:var 0]
     [:var 'int-mult]
     :apply
     [;; Binds to local variable
      [:var 1]
      [:var 1]
      [:var 'int-add]
      :apply]
     :let])

  (def code
    (c/push->clj {:push     push
                  :inputs   ['in1]
                  :ret-type int?
                  :type-env (conj lib/environment [:= 'in1 int?])
                  :dealiases lib/dealiases}))

  code

  (def square&double
    (c/synth-fn ['in1] code))

  (doseq [i (range 5)]
    (println i (square&double i)))

  `(fn ~(vec ['in1]) ~code)
  ;(fn [in1]
  ;  (let [v-16674 (* in1 in1)]
  ;    (+ v-16674 v-16674)))

  )

(comment
  ;; Anonymous function abstraction
  ;; Higher order functions

  (def push
    [[:lit [1 2 3]]
     [;; Binds to local variable
      [:var 0]
      [:var 'int-inc]
      :apply]
     [:fn int?]
     [:var 'mapv]
     :apply])

  (def code
    (c/push->clj {:push     push
                  :inputs   []
                  :ret-type [:vector int?]
                  :type-env lib/environment
                  :dealiases lib/dealiases}))

  (def inc-all
    (c/synth-fn [] code))

  (println (inc-all))

  `(fn ~(vec ['in1]) ~code)
  ;; (fn [in1] (mapv (fn [a-9253] (inc a-9253)) [1 2 3]))

  )
