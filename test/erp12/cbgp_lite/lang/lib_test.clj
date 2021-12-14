(ns erp12.cbgp-lite.lang.lib-test
  (:require [clojure.test :refer :all]
            [erp12.cbgp-lite.lang.lib :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(deftest mapcatv-test
  (is (= [:a :b] (mapcatv identity [[:a] [:b]]))))

(deftest removev-test
  (is (= [:a :b] (removev #{:_} [:a :_ :b]))))

(deftest string-concat-test
  (is (= "abcdef" (string-concat "abc" "def"))))

(deftest append-str-test
  (is (= "abc" (append-str "ab" "c")))
  (is (= "abc" (append-str "ab" \c))))

(deftest take-str-test
  (is (= "ab" (take-str 2 "abcd"))))

(deftest rest-str-test
  (is (= "bcd" (rest-str "abcd"))))

(deftest butlast-str-test
  (is (= "abc" (butlast-str "abcd"))))

(deftest split-str-test
  (is (= ["a" "c"] (split-str "abc" "b"))))

(deftest split-str-on-char-test
  (is (= ["a" "c"] (split-str-on-char "abc" \b))))

(deftest split-str-on-ws-test
  (is (= ["abc" "def"] (split-str-on-ws "abc def"))))

(deftest substring?-test
  (is (substring? "abc" "b")))

(deftest contains-char?-test
  (is (contains-char? "abc" \b)))

(deftest index-of-char-test
  (is (= 1 (index-of-char "abc" \b))))

(deftest occurrences-of-char-test
  (is (= 1 (occurrences-of-char "abc" \b))))

(deftest replace-char-test
  (is (= "zzz" (replace-char "aaa" \a \z))))

(deftest replace-first-char-test
  (is (= "zaa" (replace-first-char "aaa" \a \z))))

(deftest remove-char-test
  (is (= "" (remove-char "aaa" \a)))
  (is (= "bc" (remove-char "abc" \a))))

(deftest set-char-test
  (is (= "aba" (set-char "aaa" 1 \b))))

(deftest concatv-test
  (is (= [1 2 3] (concatv [1 2] [3]))))

(deftest takev-test
  (is (= [1] (takev 1 [1 2 3]))))

(deftest restv-test
  (is (= [2 3] (restv [1 2 3]))))

(deftest butlastv-test
  (is (= [1 2] (butlastv [1 2 3]))))

(deftest reversev-test
  (is (= [3 2 1] (reversev [1 2 3]))))

(deftest in?-test
  (is (in? [:a :b :c] :b))
  (is (not (in? [:a :b :c] :z))))

(deftest index-of-test
  (is (= 1 (index-of [:a :b :c] :b)))
  (is (= -1 (index-of [:a :b :c] :z))))

(deftest occurrences-of-test
  (is (= 3 (occurrences-of [:a :a :a] :a)))
  (is (= 0 (occurrences-of [:a :b :c] :z)))
  (is (= 0 (occurrences-of [] :z))))

(deftest replacev-test
  (is (= [:a :_ :c :_]
         (replacev [:a :b :c :b] :b :_))))

(deftest replacev-first-test
  (is (= [:a :_ :c :b]
         (replacev-first [:a :b :c :b] :b :_))))

(deftest remove-element-test
  (is (= [:a :c]
         (remove-element [:a :b :c] :b))))

(deftest safe-subs-test
  (is (= "abc" (safe-subs "abc" -1 1000)))
  (is (= "" (safe-subs "abc" 10 1))))

(deftest safe-subvec-test
  (is (= [:a] (safe-subvec [:a] -1 10)))
  (is (= [] (safe-subvec [:a] 10 1))))

(deftest safe-assoc-test
  (is (= [:_ :b] (safe-assoc [:a :b] 2 :_))))

(deftest safe-nth-test
  (is (= :b (safe-nth [:a :b] 3)))
  (is (thrown? ExceptionInfo
               (safe-nth [] 0))))
