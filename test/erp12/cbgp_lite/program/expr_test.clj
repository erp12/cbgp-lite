(ns erp12.cbgp-lite.program.expr-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.cbgp-lite.program.expr :refer :all]
            [erp12.cbgp-lite.program.types :as t]
            ;; adds support for `match?` and `thrown-match?` in `is` expressions
            [matcher-combinators.test]))

(declare match?)

(def type-env
  {`+     (t/fn-scheme {:args [t/INT t/INT]
                        :ret  t/INT})
   `count (t/fn-scheme {:kinds [:*]
                        :args  [(t/vec-type 0)]
                        :ret   t/INT})})

(deftest infer-type-test
  (is (= t/INT
         (infer-type (->Lit 1 t/INT)
                     type-env)))
  (testing "Var"
    (is (= (t/fn-type [t/INT t/INT]
                      t/INT)
           (infer-type (->Var `+)
                       type-env)))
    (is (match? (t/fn-type [(t/vec-type (t/->TypeVar symbol? :*))]
                           t/INT)
                (infer-type (->Var `count)
                            type-env))))
  (testing "App"
    (is (= t/INT
           (infer-type (->App (->Var `+)
                              [(->Lit 1 t/INT)
                               (->Lit 2 t/INT)])
                       type-env))))
  (testing "Abs"
    (is (= (t/fn-type [t/INT t/INT] t/INT)
           (infer-type (->Abs ['a 'b]
                              (->App (->Var `+)
                                     [(->Var 'a)
                                      (->Var 'b)]))
                       type-env)))
    (is (match? (t/fn-type [(t/->TypeVar symbol? :*)] (t/->TypeVar symbol? :*))
                (infer-type (->Abs ['a] (->Var 'a))
                            type-env))))
  (testing "Let"
    (is (= t/INT
           (infer-type (->Let [{:sym 'x
                                :def (->Lit 1 t/INT)}]
                              (->Var 'x))
                       type-env)))
    (is (= t/INT
           (infer-type (->Let [{:sym 'double
                                :def (->Abs ['a]
                                            (->App (->Var `+)
                                                   [(->Var 'a)
                                                    (->Var 'a)]))}]
                              (->App (->Var 'double)
                                     [(->Lit 2 t/INT)]))
                       type-env)))))

(deftest guarded-fn-test
  (let [f (guarded-fn [x] (vec (range x)))]
    (is (= [0 1 2] (f 3)))
    (is (thrown? clojure.lang.ExceptionInfo (f 1e6)))))

(deftest abs-to-form-test
  (is (= '(erp12.cbgp-lite.program.expr/guarded-fn [x] 
                                                   (clojure.core/vec (clojure.core/range x)))
         (to-form (->Abs ['x]
                         (->App (->Var `vec)
                                [(->App (->Var `range)
                                        [(->Var 'x)])]))))))

(deftest de-bruijn-test
  (testing "Lit"
    (is (= {:kind :Lit, :node 1}
           (de-bruijn (->Lit 1 t/INT)))))
  (testing "Var (free)"
    (is (= {:kind :Var, :node 'x}
           (de-bruijn (->Var 'x)))))
  (testing "App"
    (is (= {:kind :App
            :children [{:kind :Var, :node 'f} 
                       {:kind :Lit, :node 1}]}
           (de-bruijn (->App (->Var 'f) [(->Lit 1 t/INT)])))))
  (testing "Abs"
    (is (= {:kind :Abs
            :children [{:kind :App
                        :children [{:kind :Var, :node 'f}
                                   {:kind :Var, :node {:de-bruijn 0}}]}]}
           (de-bruijn (->Abs ['x] (->App (->Var 'f) [(->Var 'x)])))))
    (is (= {:kind    :Abs
            :children [{:kind    :App
                        :children [{:kind :Var, :node {:de-bruijn 1}}
                                   {:kind :Var, :node {:de-bruijn 0}}]}]}
           (de-bruijn (->Abs ['x 'y] (->App (->Var 'x) [(->Var 'y)])))))
    (testing "nested"
      (is (= {:kind :Abs
              :children [{:kind :Abs
                          :children [{:kind :App
                                      :children [{:kind :Var, :node 'f}
                                                 {:kind :Var, :node {:de-bruijn 1}}
                                                 {:kind :Var, :node {:de-bruijn 0}}]}]}]}
             (de-bruijn (->Abs ['x] 
                               (->Abs ['y] 
                                      (->App (->Var 'f) 
                                             [(->Var 'x) (->Var 'y)]))))))))
  (testing "Let"
    (is (= {:kind :Let
            :children [{:kind :Lit, :node 1}
                       {:kind :Var, :node {:de-bruijn 0}}]}
           (de-bruijn (->Let [{:sym 'x :def (->Lit 1 t/INT)}]
                             (->Var 'x)))))
    (testing "dependent bindings"
      (is (= {:kind :Let
              :children [{:kind :Lit, :node 1}
                         {:kind :Var, :node {:de-bruijn 0}}
                         {:kind :Var, :node {:de-bruijn 0}}]}
             (de-bruijn (->Let [{:sym 'x :def (->Lit 1 t/INT)}
                                {:sym 'y :def (->Var 'x)}]
                               (->Var 'y))))))))


(deftest tree-edit-distance-test
  (let [tree        (de-bruijn (->Let [{:sym 'double
                                        :def (->Abs ['a]
                                                    (->App (->Var `+)
                                                           [(->Var 'a)
                                                            (->Var 'a)]))}]
                                      (->App (->Var 'double)
                                             [(->Lit 2 t/INT)])))
        tree-1-away (de-bruijn (->Let [{:sym 'square
                                        :def (->Abs ['a]
                                                    (->App (->Var `*)
                                                           [(->Var 'a)
                                                            (->Var 'a)]))}]
                                      (->App (->Var 'square)
                                             [(->Lit 2 t/INT)])))
        tree-2-away (de-bruijn  (->Let [{:sym 'square
                                         :def (->Abs ['a]
                                                     (->App (->Var `*)
                                                            [(->Var 'a)
                                                             (->Var 'a)]))}]
                                       (->App (->Var 'square)
                                              [(->Lit 3 t/INT)])))]
    (is (= 0 (tree-edit-distance tree tree)))
    (is (= 1 (tree-edit-distance tree tree-1-away)))
    (is (= 2 (tree-edit-distance tree tree-2-away)))
    (is (= 1 (tree-edit-distance tree-1-away tree-2-away)))))

(deftest tree-edit-distance-edge-cases-test
  (let [lit-1 (de-bruijn (->Lit 1 t/INT))
        lit-2 (de-bruijn (->Lit 2 t/INT))
        var-x (de-bruijn (->Var 'x))]
    
    (testing "Empty tree"
      ;; Cost to insert/delete (Lit 1) is size(1) = 1
      (is (= 1 (tree-edit-distance lit-1 nil)))
      (is (= 1 (tree-edit-distance nil lit-1))))

    (testing "Replace a leaf with the same kind of node but different value"
      ;; Lit 1 -> Lit 2. Same kind, diff value. Cost = 1.
      (is (= 1 (tree-edit-distance lit-1 lit-2))))

    (testing "Replacing node with a different kind of node"
      ;; Lit 1 -> Var x. Different kind.
      ;; Cost = delete(Lit 1) + insert(Var x)
      ;;      = 1 + 1 = 2
      (is (= 2 (tree-edit-distance lit-1 var-x))))

    (testing "Insert a child"
      ;; (f 1) -> (f 1 2)
      ;; Diff is inserting (Lit 2). Cost = 1.
      (let [f-1   (de-bruijn (->App (->Var 'f) [(->Lit 1 t/INT)]))
            f-1-2 (de-bruijn (->App (->Var 'f) [(->Lit 1 t/INT) (->Lit 2 t/INT)]))]
        (is (= 1 (tree-edit-distance f-1 f-1-2)))))

    (testing "Reorder children"
      ;; (f 1 2) -> (f 2 1)
      ;; Matches (Lit 1)->(Lit 2) [cost 1] and (Lit 2)->(Lit 1) [cost 1].
      ;; Total = 2.
      (let [f-1-2 (de-bruijn (->App (->Var 'f) [(->Lit 1 t/INT) (->Lit 2 t/INT)]))
            f-2-1 (de-bruijn (->App (->Var 'f) [(->Lit 2 t/INT) (->Lit 1 t/INT)]))]
        (is (= 2 (tree-edit-distance f-1-2 f-2-1)))))))