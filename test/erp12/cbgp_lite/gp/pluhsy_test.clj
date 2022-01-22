(ns erp12.cbgp-lite.gp.pluhsy-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.cbgp-lite.gp.pluhsy :refer :all]))

(deftest random-gene-test
  (let [opts {:gene-distribution {:open-close    0.2
                                  :input         0.2
                                  :var           0.2
                                  :lit           0.2
                                  :lit-generator 0.1
                                  :abstraction   0.1}
              :inputs            ['x]
              :vars              ['+ '- '* '/]
              :lits              [1 2 10]
              :lit-generators    [rand]
              :abstract          [:let]}]
    (testing "Only valid genes are generated"
      (doseq [gene (repeatedly 1000 #(random-gene opts))]
        (is (or (contains? #{:open :close :let} gene)
                (and (vector? gene)
                     (contains? #{:lit :var} (first gene))
                     (some? (second gene)))))))
    (testing "Random generation follows the distribution"
      (is (= (repeat 100 [:var 'x])
             (repeatedly 100 #(random-gene (assoc opts
                                             :gene-distribution
                                             {:input 1.0
                                              :var   0.0
                                              :lit   0.0}))))))))

(deftest plushy->push-test
  (is (= [:a [:b [:c] []]]
         (plushy->push '(:a :open :b :open :c :close :open))))
  (is (= '[:a [:b [[[:c]]]]]
         (plushy->push '(:a :open :b :open :open :open :c :close))))
  (is (= '[:a [:b] :c]
         (plushy->push '(:a :open :b :close :close :close :c :close)))))
