(ns erp12.cbgp-lite.utils-test
  (:require [clojure.test :refer [deftest is testing]])
  (:require [erp12.cbgp-lite.utils :refer :all]))

(deftest first-non-nil-test
  (is (= 1 (first-non-nil [nil 1 nil 2])))
  (is (= :x (first-non-nil '(nil :x))))
  (is (= :x (first-non-nil [:x nil :z]))))

(deftest safe-rand-nth-test
  (is (nil? (safe-rand-nth []))))

(deftest enhance-test
  (is (= {:a 0} (enhance {} :a count)))
  (is (= {:a 0 :b 1}
         (enhance {}
                  :a count
                  :b (fn [{:keys [a]}] (inc a))))))
