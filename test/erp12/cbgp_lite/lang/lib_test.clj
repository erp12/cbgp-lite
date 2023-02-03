(ns erp12.cbgp-lite.lang.lib-test
  (:require [clojure.test :refer [deftest testing is]]
            [erp12.cbgp-lite.lang.lib :as l])
  (:import (clojure.lang ExceptionInfo)))

(deftest mapcatv-test
  (is (= [:a :b] (l/mapcatv identity [[:a] [:b]]))))

(deftest removev-test
  (is (= [:a :b] (l/removev #{:_} [:a :_ :b]))))

(deftest concat-str-test
  (is (= "abcdef" (l/concat-str "abc" "def"))))

(deftest take-str-test
  (is (= "ab" (l/take-str 2 "abcd"))))

(deftest rest-str-test
  (is (= "bcd" (l/rest-str "abcd"))))

(deftest butlast-str-test
  (is (= "abc" (l/butlast-str "abcd"))))

(deftest split-str-test
  (is (= ["a" "c"] (l/split-str "abc" "b"))))

(deftest split-str-on-ws-test
  (is (= ["abc" "def"] (l/split-str-on-ws "abc def"))))

(deftest filter-str-test
  (is (= "ac" (l/filter-str #(not= % \b) "abc"))))

(deftest char-in?-test
  (is (l/char-in? "abc" \b)))

(deftest char-occurrences-test
  (is (= 1 (l/char-occurrences "abc" \b))))

(deftest replace-char-test
  (is (= "zzz" (l/replace-char "aaa" \a \z))))

(deftest replace-first-char-test
  (is (= "zaa" (l/replace-first-char "aaa" \a \z))))

(deftest remove-char-test
  (is (= "" (l/remove-char "aaa" \a)))
  (is (= "bc" (l/remove-char "abc" \a))))

(deftest set-char-test
  (is (= "aba" (l/set-char "aaa" 1 \b))))

(deftest concatv-test
  (is (= [1 2 3] (l/concatv [1 2] [3]))))

(deftest takev-test
  (is (= [1] (l/takev 1 [1 2 3]))))

(deftest restv-test
  (is (= [2 3] (l/restv [1 2 3]))))

(deftest butlastv-test
  (is (= [1 2] (l/butlastv [1 2 3]))))

(deftest reversev-test
  (is (= [3 2 1] (l/reversev [1 2 3]))))

(deftest in?-test
  (is (l/in? [:a :b :c] :b))
  (is (not (l/in? [:a :b :c] :z))))

(deftest index-of-test
  (is (= 1 (l/index-of [:a :b :c] :b)))
  (is (= -1 (l/index-of [:a :b :c] :z))))

(deftest occurrences-of-test
  (is (= 3 (l/occurrences-of [:a :a :a] :a)))
  (is (= 0 (l/occurrences-of [:a :b :c] :z)))
  (is (= 0 (l/occurrences-of [] :z))))

(deftest replacev-test
  (is (= [:a :_ :c :_]
         (l/replacev [:a :b :c :b] :b :_))))

(deftest replacev-first-test
  (is (= [:a :_ :c :b]
         (l/replacev-first [:a :b :c :b] :b :_))))

(deftest remove-element-test
  (is (= [:a :c]
         (l/remove-element [:a :b :c] :b))))

(deftest safe-subs-test
  (is (= "abc" (l/safe-subs "abc" -1 1000)))
  (is (= "" (l/safe-subs "abc" 10 1))))

(deftest safe-subvec-test
  (is (= [:a] (l/safe-subvec [:a] -1 10)))
  (is (= [] (l/safe-subvec [:a] 10 1))))

(deftest safe-assoc-test
  (is (= [:_ :b] (l/safe-assoc-nth [:a :b] 2 :_))))

(deftest safe-nth-test
  (is (= :b (l/safe-nth [:a :b] 3)))
  (is (thrown? ExceptionInfo
               (l/safe-nth [] 0))))
