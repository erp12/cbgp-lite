(ns erp12.cbgp-lite.utils-test
  (:require [clojure.test :refer [deftest is testing]])
  (:require [erp12.cbgp-lite.utils :refer :all]))

(deftest first-non-nil-test
  (is (= 1 (first-non-nil [nil 1 nil 2])))
  (is (= :x (first-non-nil '(nil :x))))
  (is (= :x (first-non-nil [:x nil :z]))))

(deftest safe-rand-nth-test
  (is (nil? (safe-rand-nth []))))
