(ns erp12.cbgp-lite.program.lib-test
  (:require [clojure.test :refer [deftest is]]
            [erp12.cbgp-lite.program.lib :as lib])
  (:import (clojure.lang ExceptionInfo)))

(def approx-delta 0.01)

(defn approx=
  [a b]
  (< (Math/abs (- a b)) approx-delta))

(deftest safe-div-test
  (is (approx= 5.0 (lib/safe-div 10 2)))
  (is (= 0 (lib/safe-div 10 0)))
  (is (approx= -5.0 (lib/safe-div -10 2)))
  (is (approx= 2.5 (lib/safe-div 5 2))))

(deftest safe-quot-test
  (is (= 3 (lib/safe-quot 10 3)))
  (is (= 0 (lib/safe-quot 10 0)))
  (is (= -3 (lib/safe-quot -10 3))))

(deftest safe-mod-test
  (is (= 1 (lib/safe-mod 10 3)))
  (is (= 0 (lib/safe-mod 10 0)))
  (is (= 2 (lib/safe-mod -10 3)))
  (is (= -2 (lib/safe-mod 10 -3))))

(deftest safe-pow-test
  (is (= 8.0 (lib/safe-pow 2 3)))
  (is (= -8.0 (lib/safe-pow -2 3)))
  (is (thrown? ExceptionInfo (lib/safe-pow -2 0.5)))
  (is (thrown? ExceptionInfo (lib/safe-pow 10 1000))))

(deftest safe-square-test
  (is (= 9.0 (lib/safe-square 3)))
  (is (= 9.0 (lib/safe-square -3)))
  (is (= 0.0 (lib/safe-square 0))))

(deftest safe-sqrt-test
  (is (= 4.0 (lib/safe-sqrt 16.0)))
  (is (= 4.0 (lib/safe-sqrt -16.0)))
  (is (= 0.0 (lib/safe-sqrt 0.0))))

(deftest safe-acos-test
  (is (approx= 1.5708 (lib/safe-acos 0.0)))
  (is (= 0.0 (lib/safe-acos 1.0)))
  (is (= 0.0 (lib/safe-acos -1.0)))
  (is (= 0.0 (lib/safe-acos 3.0)))
  (is (= 0.0 (lib/safe-acos -3.0))))

(deftest safe-asin-test
  (is (= 0.0 (lib/safe-asin 0.0)))
  (is (approx= 1.5708 (lib/safe-asin 1.0)))
  (is (approx= 1.5708 (lib/safe-asin -1.0)))
  (is (approx= 1.5708 (lib/safe-asin 3.0)))
  (is (approx= 1.5708 (lib/safe-asin -3.0)))
  (is (= 0.0 (lib/safe-asin 4.0))))

(deftest safe-log2-test
  (is (= 3.0 (lib/safe-log2 8.0)))
  (is (= 1.0 (lib/safe-log2 2.0)))
  (is (approx= -149.0 (lib/safe-log2 -1.0)))
  (is (approx= -149.0 (lib/safe-log2 0.0))))

(deftest safe-log10-test
  (is (= 2.0 (lib/safe-log10 100.0)))
  (is (= 1.0 (lib/safe-log10 10.0)))
  (is (approx= -44.85 (lib/safe-log10 -1.0)))
  (is (approx= -44.85 (lib/safe-log10 0.0))))

(deftest int->char-test
  (is (= \A (lib/int->char 65)))
  (is (= \a (lib/int->char 97)))
  (is (= \H (lib/int->char 200)))
  (is (= \v (lib/int->char -10)))
  (is (= (char 0) (lib/int->char 128))))

(deftest split-str-test
  (is (= ["hello" "world"] (lib/split-str "hello world" " ")))
  (is (= ["he" "" "o"] (lib/split-str "hello" "l")))
  (is (= ["a" "b"] (lib/split-str "a*b" "*")))
  (is (= ["hello"] (lib/split-str "hello" "z")))
  (is (= ["" "a)"] (lib/split-str "(a)" "("))))

(deftest split-str-on-ws-test
  (is (= ["hello" "world"] (lib/split-str-on-ws "hello world")))
  (is (= ["hello" "world"] (lib/split-str-on-ws "  hello  world  ")))
  (is (= ["a" "b" "c"] (lib/split-str-on-ws "a  b   c")))
  (is (= [""] (lib/split-str-on-ws "")))
  (is (= ["hello"] (lib/split-str-on-ws "   hello   "))))

(deftest index-of-test
  (is (= 1 (lib/index-of [:a :b :c] :b)))
  (is (= -1 (lib/index-of [:a :b :c] :z)))
  (is (= 1 (lib/index-of [\h \e \l \l \o] \e)))
  (is (= -1 (lib/index-of [1 2 3] "2")))
  (is (= 0 (lib/index-of [:a :a :a] :a)))
  (is (= -1 (lib/index-of [] :a))))

(deftest occurrences-test
  (is (= 3 (lib/occurrences [:a :a :a] :a)))
  (is (= 1 (lib/occurrences [:a :b :c] :a)))
  (is (= 0 (lib/occurrences [:a :b :c] :z)))
  (is (= 0 (lib/occurrences [] :a)))
  (is (= 3 (lib/occurrences [1 2 1 3 1] 1))))

(deftest vec-contains?-test
  (is (true? (lib/vec-contains? [:a :b :c] :b)))
  (is (false? (lib/vec-contains? [:a :b :c] :z)))
  (is (false? (lib/vec-contains? [] :a)))
  (is (true? (lib/vec-contains? [1 2 3] 2)))
  (is (true? (lib/vec-contains? [\a \b \c] \b))))

(deftest safe-nth-test
  (is (= :b (lib/safe-nth [:a :b :c] 1)))
  (is (= :c (lib/safe-nth [:a :b :c] 5)))
  (is (= :c (lib/safe-nth [:a :b :c] -1)))
  (is (= :a (lib/safe-nth [:a :b :c] 3)))
  (is (thrown? ExceptionInfo (lib/safe-nth [] 0))))

(deftest set-char-test
  (is (= "hzllo" (lib/set-char "hello" 1 \z)))
  (is (= "abz" (lib/set-char "abc" 5 \z)))
  (is (= "" (lib/set-char "" 0 \z)))
  (is (= "az" (lib/set-char "ab" -1 \z)))
  (is (= "hello" (lib/set-char "hello" 0 \h))))

(deftest safe-assoc-nth-test
  (is (= [:a :z :c] (lib/safe-assoc-nth [:a :b :c] 1 :z)))
  (is (= [:a :z] (lib/safe-assoc-nth [:a :b] 5 :z)))
  (is (= [] (lib/safe-assoc-nth [] 0 :z)))
  (is (= [:a :z] (lib/safe-assoc-nth [:a :b] -1 :z)))
  (is (= [:a :b :z] (lib/safe-assoc-nth [:a :b :c] 2 :z))))

(deftest assoc-tuple-test
  (is (= [:a nil] (lib/assoc-tuple nil 0 :a)))
  (is (= [:a :z] (lib/assoc-tuple [:a :b] 1 :z)))
  (is (= [:z :b] (lib/assoc-tuple [:a :b] 0 :z)))
  (is (= [:a :b] (lib/assoc-tuple [:a :b] 0 :a)))
  (is (= [:a :b] (lib/assoc-tuple [:a :b] 1 :b))))

(deftest safe-subs-test
  (is (= "ell" (lib/safe-subs "hello" 1 4)))
  (is (= "hell" (lib/safe-subs "hello" -1 4)))
  (is (= "hello" (lib/safe-subs "hello" 0 100)))
  (is (= "" (lib/safe-subs "hello" 4 1)))
  (is (= "" (lib/safe-subs "" 0 5)))
  (is (= "lo" (lib/safe-subs "hello" 3 10)))
  (is (= "llo" (lib/safe-subs "hello" 2 100))))

(deftest safe-subvec-test
  (is (= [2 3 4] (lib/safe-subvec [1 2 3 4 5] 1 4)))
  (is (= [1 2 3] (lib/safe-subvec [1 2 3] -1 3)))
  (is (= [1 2 3] (lib/safe-subvec [1 2 3] 0 100)))
  (is (= [] (lib/safe-subvec [1 2 3] 4 1)))
  (is (= [] (lib/safe-subvec [] 0 5)))
  (is (= [3 4 5] (lib/safe-subvec [1 2 3 4 5] 2 10)))
  (is (= [2 3] (lib/safe-subvec [1 2 3] 1 3))))

(deftest vec-replace-test
  (is (= [:a :z :c :z] (lib/vec-replace [:a :b :c :b] :b :z)))
  (is (= [:a :z :c] (lib/vec-replace [:a :b :c] :b :z)))
  (is (= [:a :b :c] (lib/vec-replace [:a :b :c] :d :z)))
  (is (= [1 2 1 3 1] (lib/vec-replace [1 2 1 3 1] 1 1)))
  (is (= [:a :a :c] (lib/vec-replace [:a :b :c] :b :a)))
  (is (= [1 2 3] (lib/vec-replace [1 2 3] 4 5))))

(deftest vec-replace-first-test
  (is (= [:a :z :c :b] (lib/vec-replace-first [:a :b :c :b] :b :z)))
  (is (= [:a :z :c] (lib/vec-replace-first [:a :b :c] :b :z)))
  (is (= [:a :b :c] (lib/vec-replace-first [:a :b :c] :d :z)))
  (is (= [:z :b :c :b] (lib/vec-replace-first [:a :b :c :b] :a :z)))
  (is (= [1 2 3] (lib/vec-replace-first [1 2 3] 4 5))))

(deftest guarded-reduce-test
  (is (thrown? clojure.lang.ExceptionInfo
               (lib/fold-vec (fn [acc _] (vec (concat acc acc)))
                             ["!"]
                             (range 20))))
  (is (= ["!" "!" "!" "!"]
         (lib/fold-vec (fn [acc _] (vec (concat acc acc)))
                       ["!"]
                       (range 2)))))
