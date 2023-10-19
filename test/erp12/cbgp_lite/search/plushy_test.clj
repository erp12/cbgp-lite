(ns erp12.cbgp-lite.search.plushy-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.cbgp-lite.search.plushy :as pl]))

(deftest random-gene-test
  (let [genes [{:gene :var :name '+}
               {:gene :var :name '-}
               {:gene :var :name '*}
               {:gene :var :name '/}
               {:gene :lit :val 1}
               {:gene :lit :val 2}
               {:gene :lit :val 10}
               {:gene :lit-generator :fn rand}
               {:gene :local}
               {:gene :apply}
               {:gene :close}]
        gene+prob (pl/prob-by-gene-kind genes
                                        {:close         0.2
                                         :local         0.2
                                         :var           0.2
                                         :lit           0.2
                                         :lit-generator 0.1
                                         :apply         0.1})

        source (pl/make-genetic-source gene+prob)]
    (testing "Only valid genes are generated"
      (doseq [gene (repeatedly 1000 source)]
        ;; @todo Consider using malli for better test
        (is (contains? #{:lit :var :local :apply :close}
                       (:gene gene)))))
    (testing "Random generation follows the distribution"
      (is (= (repeat 100 {:gene :apply})
             (repeatedly 100 (pl/make-genetic-source (pl/prob-by-gene-kind [{:gene :apply}] {:apply 1}))))))))

(deftest plushy->push-test
  (is (= (pl/plushy->push '({:gene :lit :val 0}
                            {:gene :let}
                            {:gene :lit :val 1}
                            {:gene :close}
                            {:gene :lit :val 2}
                            {:gene :fn :arg-types [:int]}))
         [{:gene :lit :val 0}
          {:gene :let}
          [{:gene :lit :val 1}]
          {:gene :lit :val 2}
          {:gene :fn :arg-types [:int]}
          []])))
