(ns erp12.cbgp-lite.lang.lib-test
  (:require [clojure.test :refer [deftest is]]
            [erp12.cbgp-lite.lang.lib :as l]
            [clojure.set :as set])
  (:import (clojure.lang ExceptionInfo)))

(deftest safe-sqrt-test
  (is (= 4.0 (l/safe-sqrt 16.0)))
  (is (= 4.0 (l/safe-sqrt -16.0))))

(deftest safe-log2-test
  (is (= 3.0 (l/safe-log2 8.0)))
  (is (= -149 (int (l/safe-log2 -1.0)))))

(deftest safe-log10-test
  (is (= 2.0 (l/safe-log10 100.0)))
  (is (= -44 (int (l/safe-log10 -1.0)))))

(deftest safe-trig-test
  (is (= 0.0 (l/safe-acos 1.0)))
  (is (= 0.0 (l/safe-acos 3.0)))
  (is (= 0.0 (l/safe-asin 0.0)))
  (is (= 0.0 (l/safe-asin 4.0))))

(deftest mapcatv-test
  (is (= [:a :b] (l/mapcat' identity [[:a] [:b]]))))

(deftest removev-test
  (is (= [:a :b] (l/remove' #{:_} [:a :_ :b]))))

(deftest concat-str-test
  (is (= "abcdef" (l/concat' "abc" "def"))))

(deftest take-str-test
  (is (= "ab" (l/take' 2 "abcd"))))

(deftest rest-str-test
  (is (= "bcd" (l/rest' "abcd"))))

(deftest butlast-str-test
  (is (= "abc" (l/butlast' "abcd"))))

(deftest str-sort-test
  (is (= "abc" (l/sort' "cba"))))

(deftest split-str-test
  (is (= ["a" "c"] (l/split-str "abc" "b"))))

(deftest split-str-on-ws-test
  (is (= ["abc" "def"] (l/split-str-on-ws "abc def"))))

(deftest filter-str-test
  (is (= "ac" (l/filter' #(not= % \b) "abc"))))

(deftest char-in?-test
  (is (l/in? "abc" \b)))

(deftest char-occurrences-test
  (is (= 1 (l/occurrences-of "abc" \b))))

(deftest replace-char-test
  (is (= "zzz" (l/replace' "aaa" \a \z))))

(deftest replace-first-char-test
  (is (= "zaa" (l/replace-first' "aaa" \a \z))))

(deftest remove-element-test
  (is (= "" (l/remove-element "aaa" \a)))
  (is (= "bc" (l/remove-element "abc" \a))))

(deftest set-char-test
  (is (= "aba" (l/set-char "aaa" 1 \b))))

(deftest concatv-test
  (is (= [1 2 3] (l/concat' [1 2] [3]))))

(deftest takev-test
  (is (= [1] (l/take' 1 [1 2 3]))))

(deftest restv-test
  (is (= [2 3] (l/rest' [1 2 3]))))

(deftest butlastv-test
  (is (= [1 2] (l/butlast' [1 2 3]))))

(deftest reversev-test
  (is (= [3 2 1] (l/reverse' [1 2 3]))))

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
         (l/replace' [:a :b :c :b] :b :_))))

(deftest replacev-first-test
  (is (= [:a :_ :c :b]
         (l/replace-first' [:a :b :c :b] :b :_))))

(deftest remove-element-test
  (is (= [:a :c]
         (l/remove-element [:a :b :c] :b)))
  (is (= [:a :b :c]
         (l/remove-element [:a :b :c] :d))))

(deftest safe-subs-test
  (is (= "abc" (l/safe-sub-coll "abc" -1 1000)))
  (is (= "" (l/safe-sub-coll "abc" 10 1)))
  (is (= [:a] (l/safe-sub-coll [:a] -1 10)))
  (is (= [] (l/safe-sub-coll [:a] 10 1))))

(deftest safe-assoc-test
  (is (= [:_ :b] (l/safe-assoc-nth [:a :b] 2 :_))))

(deftest safe-nth-test
  (is (= :b (l/safe-nth [:a :b] 3)))
  (is (thrown? ExceptionInfo
               (l/safe-nth [] 0))))

(deftest all-dealiases-in-type-env-test
  (is (empty? (set/difference (set (keys l/dealiases))
                              (set (keys l/type-env))))))

(deftest lib-for-type-ctors-test
  (is (empty? (keys (l/lib-for-type-ctors #{'boolean?}))))
  (is (= #{'comp 'partial}
         (set (keys (l/lib-for-type-ctors #{:=>})))))
  (is (= #{'= `l/and 'not= `l/>' `l/or 'if `l/>=' `l/<=' 'not `l/<' `l/min' `l/max'}
         (set/difference (set (keys (l/lib-for-type-ctors #{:=> 'boolean?})))
                         (set (keys (l/lib-for-type-ctors #{:=>}))))))

  (is (= '(* +
             -
             ->vector1
             ->vector2
             ->vector3
             abs
             comp
             count
             dec
             first
             fold
             inc
             last
             mapv
             mod
             nth-or-else
             partial
             quot
             range1
             range2
             range3
             reduce
             erp12.cbgp-lite.lang.lib/butlast'
             erp12.cbgp-lite.lang.lib/concat'
             erp12.cbgp-lite.lang.lib/conj'
             erp12.cbgp-lite.lang.lib/distinctv
             erp12.cbgp-lite.lang.lib/index-of
             erp12.cbgp-lite.lang.lib/map2v
             erp12.cbgp-lite.lang.lib/mapcat'
             erp12.cbgp-lite.lang.lib/mapv-indexed
             erp12.cbgp-lite.lang.lib/max'
             erp12.cbgp-lite.lang.lib/min'
             erp12.cbgp-lite.lang.lib/neg
             erp12.cbgp-lite.lang.lib/occurrences-of
             erp12.cbgp-lite.lang.lib/pow
             erp12.cbgp-lite.lang.lib/remove-element
             erp12.cbgp-lite.lang.lib/replace'
             erp12.cbgp-lite.lang.lib/replace-first'
             erp12.cbgp-lite.lang.lib/rest'
             erp12.cbgp-lite.lang.lib/reverse'
             erp12.cbgp-lite.lang.lib/safe-assoc-nth
             erp12.cbgp-lite.lang.lib/safe-nth
             erp12.cbgp-lite.lang.lib/safe-sub-coll
             erp12.cbgp-lite.lang.lib/sort'
             erp12.cbgp-lite.lang.lib/sortv-by
             erp12.cbgp-lite.lang.lib/square
             erp12.cbgp-lite.lang.lib/take')
         (sort
          (keys
           (l/lib-for-type-ctors #{'int? :vector :=>})))))

  (is (= '(->vector1 ->vector2
                     ->vector3
                     =
                     comp
                     empty?
                     first
                     fold
                     if
                     last
                     mapv
                     not
                     not=
                     partial
                     reduce
                     erp12.cbgp-lite.lang.lib/<'
                     erp12.cbgp-lite.lang.lib/<='
                     erp12.cbgp-lite.lang.lib/>'
                     erp12.cbgp-lite.lang.lib/>='
                     erp12.cbgp-lite.lang.lib/and
                     erp12.cbgp-lite.lang.lib/butlast'
                     erp12.cbgp-lite.lang.lib/concat'
                     erp12.cbgp-lite.lang.lib/conj'
                     erp12.cbgp-lite.lang.lib/distinctv
                     erp12.cbgp-lite.lang.lib/filter'
                     erp12.cbgp-lite.lang.lib/in?
                     erp12.cbgp-lite.lang.lib/map2v
                     erp12.cbgp-lite.lang.lib/mapcat'
                     erp12.cbgp-lite.lang.lib/max'
                     erp12.cbgp-lite.lang.lib/min'
                     erp12.cbgp-lite.lang.lib/or
                     erp12.cbgp-lite.lang.lib/remove'
                     erp12.cbgp-lite.lang.lib/remove-element
                     erp12.cbgp-lite.lang.lib/replace'
                     erp12.cbgp-lite.lang.lib/replace-first'
                     erp12.cbgp-lite.lang.lib/rest'
                     erp12.cbgp-lite.lang.lib/reverse'
                     erp12.cbgp-lite.lang.lib/sort'
                     erp12.cbgp-lite.lang.lib/sortv-by)
         (sort
          (keys
           (l/lib-for-type-ctors #{:vector 'boolean? :=>})))))

  (is (= '(= comp
             if
             not
             not=
             partial
             erp12.cbgp-lite.lang.lib/<'
             erp12.cbgp-lite.lang.lib/<='
             erp12.cbgp-lite.lang.lib/>'
             erp12.cbgp-lite.lang.lib/>='
             erp12.cbgp-lite.lang.lib/and
             erp12.cbgp-lite.lang.lib/max'
             erp12.cbgp-lite.lang.lib/min'
             erp12.cbgp-lite.lang.lib/or)
         (sort
          (keys
           (l/lib-for-type-ctors #{'boolean? :=>}))))))