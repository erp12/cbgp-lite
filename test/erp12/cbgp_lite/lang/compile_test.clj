(ns erp12.cbgp-lite.lang.compile-test
  (:require [clojure.test :refer :all]
            [erp12.cbgp-lite.lang.compile :refer :all]
            [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.search.pluhsy :as pl]))

(def environment
  (mapv (fn [[symb annotation]] [:= symb annotation])
        lib/library))


(deftest box-ast-test
  (is (= {:ast [:lit 1] :type int?}
         (box-ast [:lit 1] [])))
  (is (= {:ast [:lit [1 2 3]] :type [:vector int?]}
         (box-ast [:lit [1 2 3]] []))))

(deftest nth-var-test
  (is (= {:ast [:var 'x] :type int?}
         (nth-var 100 {:vars [{:ast [:var 'x] :type int?}]})))
  (is (= nil (nth-var 100 {:vars []}))))

(deftest macro?-test
  (is (macro? [:var 'if]))
  (is (not (macro? [:var 'float-add]))))

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
                                           {:ast :_ :type [:=> [:cat int?] string?]})}))))
  (testing "skip macros"
    (is (= {:ast   {:ast [:var '+] :type :_}
            :state {:asts (list {:ast [:var 'if] :type :_})}}
           (pop-ast {:asts (list {:ast [:var 'if] :type :_}
                                 {:ast [:var '+] :type :_})})))))

(deftest simple-math-test
  ;; Add 100 to the input.
  (let [push [[:lit 100]
              [:var 'in1]
              [:var 'int-add]
              :apply]
        code (push->clj {:push        push
                         :arg-symbols ['in1]
                         :return-type int?
                         :type-env    (conj environment [:= 'in1 int?])
                         :dealiases   lib/dealiases})
        f (synth-fn ['in1] code)]
    (is (= 100 (f 0)))
    (is (= 200 (f 100)))
    ;; @todo How test the contents of `code` with random variable names?
    ))

(deftest conditional-logic-test
  ;; If input < 1000, return "small" else "large".
  (is (= '(if (< in1 1000) "small" "large")
         (push->clj {:push        [[:lit "large"]
                                   [:lit "small"]
                                   [:lit 1000]
                                   [:var 0]
                                   [:var 'int-lt]
                                   :apply
                                   [:var 'if]
                                   :apply]
                     :arg-symbols ['in1]
                     :return-type string?
                     :type-env    (conj environment [:= 'in1 int?])
                     :dealiases   lib/dealiases}))))

(deftest let-binding-test
  ;; Square and then double the input.
  (let [push [;; [:var 0] Binds to 'in1
              [:var 0]
              [:var 0]
              [:var 'int-mult]
              :apply
              :let
              [;; Binds to local variable
               [:var 1]
               [:var 1]
               [:var 'int-add]
               :apply]]
        code (push->clj {:push        push
                         :arg-symbols ['in1]
                         :return-type int?
                         :type-env    (conj environment [:= 'in1 int?])
                         :dealiases   lib/dealiases})
        f (synth-fn ['in1] code)]
    (is (= 0 (f 0)))
    (is (= 2 (f 1)))
    (is (= 8 (f 2)))))

(deftest hof-with-anonymous-fn-test
  ;; Map `inc` over the elements of a vector
  (let [push [[:lit [1 2 3]]
              [:fn int?]
              [;; Binds to local function argument
               [:var 0]
               [:var 'int-inc]
               :apply]
              [:var 'mapv]
              :apply]
        code (push->clj {:push        push
                         :arg-symbols []
                         :return-type [:vector int?]
                         :type-env    environment
                         :dealiases   lib/dealiases})
        f (synth-fn [] code)]
    (is (= [2 3 4] (f)))))

(deftest nullary-fn-test
  (is (= '(repeatedly 5 (clojure.core/fn [] (rand)))
         (push->clj {:push        [[:var 'rand]
                                   :apply
                                   [:fn]
                                   [:lit 5]
                                   [:var 'repeatedly]
                                   :apply]
                     :arg-symbols []
                     :return-type [:vector float?]
                     :type-env    [[:= 'rand [:=> [:cat] float?]]
                                   [:= 'repeatedly {:s-vars '[a]
                                                    :body   [:=> [:cat int? [:=> [:cat] [:s-var 'a]]]
                                                             [:vector [:s-var 'a]]]}]]
                     :dealiases   {}}))))


(deftest side-effects-test
  (is (= '(do (println "Hello world!") 0)
         (push->clj {:push        [[:lit 0]
                                   [:lit "Hello world!"]
                                   [:var 'println]
                                   :apply
                                   [:var 'do2]
                                   :apply]
                     :arg-symbols []
                     :return-type int?
                     :type-env    (mapv (fn [[symb typ]] [:= symb typ]) lib/library)
                     :dealiases   lib/dealiases}))))

(deftest replace-space-with-newline-test
  (let [push [[:lit \newline]
              [:lit \space]
              [:var 0]
              [:var `lib/replace-char]
              :apply
              :let
              ;; From here below is the body of the `let`.
              ;; Do part 2
              [:lit \newline]
              [:var 1]
              [:var `lib/remove-char]
              :apply
              [:var 'length]
              :apply
              ;; Do part 1
              [:var 1]
              [:var 'println]
              :apply
              ;; Do
              [:var 'do2]
              :apply]
        func (c/synth-fn
               ['input1]
               (c/push->clj {:push        (pl/plushy->push push)
                             :arg-symbols ['input1]
                             :return-type int?
                             :type-env    (->> lib/library
                                               (merge {'input1 string?})
                                               (mapv (fn [[symb typ]] [:= symb typ])))
                             :dealiases   lib/dealiases}))]
    (is (= "Hello\nworld!\n"
           (with-out-str
             (is (= 11 (func "Hello world!"))))))))
