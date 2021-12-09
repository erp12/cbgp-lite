(ns erp12.cbgp-lite.lang.compile-test
  (:require [clojure.test :refer :all]
            [erp12.cbgp-lite.lang.compile :refer :all]
            [clojure.core.match :refer [match]]
            [erp12.cbgp-lite.lang.lib :as lib]))


(deftest box-ast-test
  (is (= {:ast [:lit 1] :type int?}
         (box-ast [:lit 1] [])))
  (is (= {:ast [:lit [1 2 3]] :type [:vector int?]}
         (box-ast [:lit [1 2 3]] []))))

(deftest nth-var-test
  (is (= {:ast [:var 'x] :type int?}
         (nth-var 100 {:vars [{:ast [:var 'x] :type int?}]})))
  (is (= nil (nth-var 100 {:vars []}))))

(deftest pop-ast-test
  (is (= {:ast :none :state empty-state}
         (pop-ast empty-state)))
  (is (= {:ast   {:ast :_ :type int?}
          :state {:asts (list)}}
         (pop-ast {:asts [{:ast :_ :type int?}]})))
  (testing "popping functions of any type"
    (is (= {:ast   {:ast :_ :type [:=> [:cat int?] string?]}
            :state {:asts (list {:ast :_ :type boolean?})}}
           (pop-function-ast {:asts (list {:ast :_ :type boolean?}
                                          {:ast :_ :type [:=> [:cat int?] string?]})})))
    (is (= {:ast   {:ast  :_
                    :type {:s-vars ['a]
                           :body   [:=> [:cat [:s-var 'a]] [:s-var 'a]]}}
            :state {:asts (list {:ast :_ :type boolean?})}}
           (pop-function-ast {:asts (list {:ast :_ :type boolean?}
                                          {:ast  :_
                                           :type {:s-vars ['a]
                                                  :body   [:=> [:cat [:s-var 'a]] [:s-var 'a]]}})}))))
  (testing "popping function of specific type"
    (is (= {:ast      {:ast :_ :type [:=> [:cat int?] string?]}
            :state    {:asts (list {:ast :_ :type boolean?})}
            :bindings {}}
           (pop-unifiable-ast [:=> [:cat int?] string?]
                              {:asts (list {:ast :_ :type boolean?}
                                           {:ast :_ :type [:=> [:cat int?] string?]})})))))

(deftest compile-chunk-test
  (is (= {:ast :none :state empty-state}
         (compile-chunk {:bound-vars []
                         :ret-type   :any
                         :state      empty-state
                         :type-env   []}))))

(deftest simple-math-test
  ;; Add 100 to the input.
  (let [push [[:lit 100]
              [:var 'in1]
              [:var 'int-add]
              :apply]
        code (push->clj {:push          push
                         :inputs        ['in1]
                         :ret-type      int?
                         :type-env      (conj lib/environment [:= 'in1 int?])
                         :alias->symbol lib/alias->symbol})
        f (synth-fn ['in1] code)]
    (is (= 100 (f 0)))
    (is (= 200 (f 100)))
    ;; @todo How test the contents of `code` with random variable names?
    ))

(deftest conditional-logic-test
  ;; If input < 1000, return "small" else "large".
  (let [push [[:lit "large"]
              [:lit "small"]
              [:lit 1000]
              [:var 0]
              [:var 'int-lt]
              :apply
              [:var 'if]
              :apply]
        code (push->clj {:push          push
                         :inputs        ['in1]
                         :ret-type      int?
                         :type-env      (conj lib/environment [:= 'in1 int?])
                         :alias->symbol lib/alias->symbol})
        f (synth-fn ['in1] code)]
    (is (= "small" (f 100)))
    (is (= "large" (f 1000)))
    (is (= "large" (f 10000)))))

(deftest let-binding-test
  ;; Square and then double the input.
  (let [push [;; [:var 0] Binds to 'in1
              [:var 0]
              [:var 0]
              [:var 'int-mult]
              :apply
              [;; Binds to local variable
               [:var 1]
               [:var 1]
               [:var 'int-add]
               :apply]
              :let]
        code (push->clj {:push          push
                         :inputs        ['in1]
                         :ret-type      int?
                         :type-env      (conj lib/environment [:= 'in1 int?])
                         :alias->symbol lib/alias->symbol})
        f (synth-fn ['in1] code)]
    (is (= 0 (f 0)))
    (is (= 2 (f 1)))
    (is (= 8 (f 2)))))

(deftest hof-with-anonymous-fn-test
  ;; Map `inc` over the elements of a vector
  (let [push [[:lit [1 2 3]]
              [;; Binds to local variable
               [:var 0]
               [:var 'int-inc]
               :apply]
              [:fn int?]
              [:var 'mapv]
              :apply]
        code (push->clj {:push          push
                         :inputs        []
                         :ret-type      [:vector int?]
                         :type-env      lib/environment
                         :alias->symbol lib/alias->symbol})
        f (synth-fn [] code)]
    (is (= [2 3 4] (f)))))
