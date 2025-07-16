(ns erp12.cbgp-lite.lang.decompile-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [erp12.cbgp-lite.lang.decompile :as de]
            [erp12.cbgp-lite.lang.lib :as lib]))
 
;; To test only this file:
;; clj -X:test :only erp12.cbgp-lite.lang.decompile-test 

(deftest decompile-constants-test
  (is (= (de/decompile-ast (ana.jvm/analyze 5))
         '({:gene :lit, :type {:type int?}, :val 5})))
  (is (= (de/decompile-ast (ana.jvm/analyze true))
         '({:gene :lit, :type {:type boolean?}, :val true})))
  (is (= (de/decompile-ast (ana.jvm/analyze nil))
         '({:gene :lit, :type {:type nil?}, :val nil})))
  (is (= (de/decompile-ast (ana.jvm/analyze 43.12))
         '({:gene :lit, :type {:type double?}, :val 43.12})))
  (is (= (de/decompile-ast (ana.jvm/analyze "hello"))
         '({:gene :lit, :type {:type string?}, :val "hello"})))
  (is (= (de/decompile-ast (ana.jvm/analyze \d))
         '({:gene :lit, :type {:type char?}, :val \d})))

;; Vectors
  (is (= '({:gene :lit, :val [1 2 7], :type {:type :vector :child {:type int?}}})
         (de/decompile-ast (ana.jvm/analyze [1 2 7]))))
  (is (= '({:gene :lit, :val [6.1 32.003], :type {:type :vector :child {:type double?}}})
         (de/decompile-ast (ana.jvm/analyze [6.1 32.003]))))
  (is (= (de/decompile-ast (ana.jvm/analyze ["hi" "there" "everyone"]))
         '({:gene :lit, :val ["hi" "there" "everyone"], :type {:type :vector :child {:type string?}}})))
  (is (= (de/decompile-ast (ana.jvm/analyze [[[true false] [false] [true true]] [[true]] [[true true true] [false false false]]]))
         '({:gene :lit, :val [[[true false] [false] [true true]] [[true]] [[true true true] [false false false]]], :type {:type :vector :child {:type :vector :child {:type :vector :child {:type boolean?}}}}})))
  (is (= (de/decompile-ast (ana.jvm/analyze []))
         '({:gene :lit, :type {:child {:sym T, :type :s-var}, :type :vector}, :val []})))

  ;; Sets
  (is (= (de/decompile-ast (ana.jvm/analyze #{8 2 0}))
         (list {:gene :lit, :val #{8 2 0}, :type {:type :set :child {:type 'int?}}})))
  (is (=
       (de/decompile-ast (ana.jvm/analyze #{}))
       '({:gene :lit, :type {:child {:sym T, :type :s-var}, :type :set}, :val #{}})))

  ;; Maps
  (is (=
       (list {:gene :lit, :type {:key {:type 'int?}, :type :map-of, :value {:type 'string?}}, :val {1 "asd", 5 "asdfff"}})
       (de/decompile-ast (ana.jvm/analyze {1 "asd" 5 "asdfff"}))))
  (is (=
       (de/decompile-ast (ana.jvm/analyze {}))
       '({:gene :lit, :type {:key {:sym T, :type :s-var}, :type :map-of, :value {:sym S, :type :s-var}}, :val {}})))

  ;; Treat quoted lists as vectors
  (is (=
       (de/decompile-ast (ana.jvm/analyze '(quote ("string" "hi"))))
       (de/decompile-ast (ana.jvm/analyze  ''("string" "hi")))
       '({:gene :lit, :type {:child {:type string?}, :type :vector}, :val ["string" "hi"]})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(quote ())))
         '({:gene :lit, :type {:child {:sym T, :type :s-var}, :type :vector}, :val []}))))

(deftest decompile-recompile-constants-test
  (is (= (map #(de/compile-debugging (de/decompile-ast (ana.jvm/analyze %))
                                     {:type %2})
              '(-5 0 2999 true nil 43.12 \d "hey hey")
              '(int? int? int? boolean? nil? double? char? string?))
         '(-5 0 2999 true nil 43.12 \d "hey hey")))

  ;; vectors
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze [1 2 7]))
                               {:type :vector :child {:type 'int?}})
         [1 2 7]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze ["hey" "hello" "yo!"]))
                               {:type :vector :child {:type 'string?}})
         ["hey" "hello" "yo!"]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze [1.3 2.45 7.0098]))
                               {:type :vector :child {:type 'double?}})
         [1.3 2.45 7.0098]))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze [1 2 7]))
                               {:type :vector :child {:type 'int?}})
         [1 2 7]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze [[[true false] [false] [true true]] [[true]] [[true true true] [false false false]]]))
                               {:type :vector :child {:type :vector :child {:type :vector :child {:type 'boolean?}}}})
         [[[true false] [false] [true true]] [[true]] [[true true true] [false false false]]]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze []))
                               {:type :vector :child (lib/s-var 'T)})
         []))

  ;; sets
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze #{8 2 0}))
                               {:type :set :child {:type 'int?}})
         #{8 2 0})
      (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze #{}))
                                   {:type :set :child (lib/s-var 'T)})
             #{})))

  ;; lists
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(quote (4 6))))
                               {:child {:type 'int?}, :type :vector})
         [4 6])
      (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(quote ())))
                                   {:type :vector :child (lib/s-var 'T)})
             [])))

  ;; maps
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze {1 "asd" 5 "asdfff"}))
                               {:key {:type 'int?}, :type :map-of, :value {:type 'string?}})
         {1 "asd" 5 "asdfff"})
      (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze {}))
                                   {:type :map-of :key (lib/s-var 'T) :value (lib/s-var 'T)})
             {})))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze {[[7 8] [9 30]] #{"this" "is"} [[45 67] [7 0]] #{"absolutely" "insane"}}))
                               {:key {:type :vector :child {:type :vector :child {:type 'int?}}}, :type :map-of, :value {:type :set :child {:type 'string?}}})
         {[[7 8] [9 30]] #{"this" "is"} [[45 67] [7 0]] #{"absolutely" "insane"}}))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze {[1] "asd" [5] "asdfff"}))
                               {:key {:type :vector :child {:type 'int?}}, :type :map-of, :value {:type 'string?}})
         {[1] "asd" [5] "asdfff"})))

(deftest decompile-function-calls-test
  ;; mathematical operations
  (is (= (de/decompile-ast (ana.jvm/analyze '(+ 22 33)))
         '({:gene :lit, :type {:type int?}, :val 33}
           {:gene :lit, :type {:type int?}, :val 22}
           {:gene :var, :name +}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(* 3 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 3}
           {:gene :var, :name *}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(/ 3 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 3}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-div}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(/ 3.5 7.0)))
         '({:gene :lit, :type {:type double?}, :val 7.0}
           {:gene :lit, :type {:type double?}, :val 3.5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-div}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(* 3.2 4.4)))
         '({:gene :lit, :type {:type double?}, :val 4.4}
           {:gene :lit, :type {:type double?}, :val 3.2}
           {:gene :var, :name *}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(quot 80 7)))
         '({:gene :lit, :type {:type int?}, :val 7}
           {:gene :lit, :type {:type int?}, :val 80}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-quot}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(quot 80.3 7.8)))
         '({:gene :lit, :type {:type double?}, :val 7.8}
           {:gene :lit, :type {:type double?}, :val 80.3}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-quot}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(+ 22.2 33.3)))
         '({:gene :lit, :type {:type double?}, :val 33.3}
           {:gene :lit, :type {:type double?}, :val 22.2}
           {:gene :var, :name +}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(+ 22 (+ 33 44))))
         '({:gene :lit, :type {:type int?}, :val 44}
           {:gene :lit, :type {:type int?}, :val 33}
           {:gene :var, :name +}
           {:gene :apply}
           {:gene :lit, :type {:type int?}, :val 22}
           {:gene :var, :name +}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(- 22 33)))
         '({:gene :lit, :type {:type int?}, :val 33}
           {:gene :lit, :type {:type int?}, :val 22}
           {:gene :var, :name -}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(- 22.5 33.5)))
         '({:gene :lit, :type {:type double?}, :val 33.5}
           {:gene :lit, :type {:type double?}, :val 22.5}
           {:gene :var, :name -}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(- 22 (- 33 44))))
         '({:gene :lit, :type {:type int?}, :val 44}
           {:gene :lit, :type {:type int?}, :val 33}
           {:gene :var, :name -}
           {:gene :apply}
           {:gene :lit, :type {:type int?}, :val 22}
           {:gene :var, :name -}
           {:gene :apply})))

  ;; comparison (<, <=, >, >=)
  (is (= (de/decompile-ast (ana.jvm/analyze '(< 4 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 4}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(< \a \b)))
         '({:gene :lit, :type {:type char?}, :val \b}
           {:gene :lit, :type {:type char?}, :val \a}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(< "hi" "there")))
         '({:gene :lit, :type {:type string?}, :val "there"}
           {:gene :lit, :type {:type string?}, :val "hi"}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(<= 4 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 4}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<='}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(<= \a \a)))
         '({:gene :lit, :type {:type char?}, :val \a}
           {:gene :lit, :type {:type char?}, :val \a}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<='}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(< "you there" "hi!")))
         '({:gene :lit, :type {:type string?}, :val "hi!"}
           {:gene :lit, :type {:type string?}, :val "you there"}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(> 5 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/>'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(> \a \b)))
         '({:gene :lit, :type {:type char?}, :val \b}
           {:gene :lit, :type {:type char?}, :val \a}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/>'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(> "hi" "there")))
         '({:gene :lit, :type {:type string?}, :val "there"}
           {:gene :lit, :type {:type string?}, :val "hi"}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/>'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(>= 5 5)))
         '({:gene :lit, :type {:type int?}, :val 5}
           {:gene :lit, :type {:type int?}, :val 5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/>='}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(>= \a \b)))
         '({:gene :lit, :type {:type char?}, :val \b}
           {:gene :lit, :type {:type char?}, :val \a}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/>='}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(>= "hi" "there")))
         '({:gene :lit, :type {:type string?}, :val "there"}
           {:gene :lit, :type {:type string?}, :val "hi"}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/>='}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(zero? (- 4 1))))
         '({:gene :lit, :type {:type int?}, :val 1}
           {:gene :lit, :type {:type int?}, :val 4}
           {:gene :var, :name -}
           {:gene :apply}
           {:gene :var, :name zero?}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(- 1)))
         '({:gene :lit, :type {:type int?}, :val 1} {:gene :var, :name erp12.cbgp-lite.lang.lib/neg} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(- 1.0)))
         '({:gene :lit, :type {:type double?}, :val 1.0} {:gene :var, :name erp12.cbgp-lite.lang.lib/neg} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(- 4 5 (- 2))))
         '({:gene :lit, :type {:type int?}, :val 2}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/neg}
           {:gene :apply}
           {:gene :lit, :type {:type int?}, :val 5}
           {:gene :lit, :type {:type int?}, :val 4}
           {:gene :var, :name -}
           {:gene :apply}
           {:gene :var, :name -}
           {:gene :apply})))

  ;;max and min
  (is (= (de/decompile-ast (ana.jvm/analyze '(max 1 2 3)))
         '({:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 1}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(max 1.3 2.2 3.9)))
         '({:gene :lit, :type {:type double?}, :val 3.9}
           {:gene :lit, :type {:type double?}, :val 2.2}
           {:gene :lit, :type {:type double?}, :val 1.3}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(max \a \g \v)))
         '({:gene :lit, :type {:type char?}, :val \v}
           {:gene :lit, :type {:type char?}, :val \g}
           {:gene :lit, :type {:type char?}, :val \a}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(max "ai" "smile" "go")))
         '({:gene :lit, :type {:type string?}, :val "go"}
           {:gene :lit, :type {:type string?}, :val "smile"}
           {:gene :lit, :type {:type string?}, :val "ai"}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(min 1 2 3)))
         '({:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 1}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(min 1.3 2.2 3.9)))
         '({:gene :lit, :type {:type double?}, :val 3.9}
           {:gene :lit, :type {:type double?}, :val 2.2}
           {:gene :lit, :type {:type double?}, :val 1.3}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(min \a \g \v)))
         '({:gene :lit, :type {:type char?}, :val \v}
           {:gene :lit, :type {:type char?}, :val \g}
           {:gene :lit, :type {:type char?}, :val \a}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(min "ai" "smile" "go")))
         '({:gene :lit, :type {:type string?}, :val "go"}
           {:gene :lit, :type {:type string?}, :val "smile"}
           {:gene :lit, :type {:type string?}, :val "ai"}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply})))

  ;; not
  (is (= (de/decompile-ast (ana.jvm/analyze '(not true)))
         '({:gene :lit, :type {:type boolean?}, :val true}
           {:gene :var, :name not}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(not (< 4 5))))
         '({:gene :lit, :type {:type int?}, :val 5}
           {:gene :lit, :type {:type int?}, :val 4}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<'}
           {:gene :apply}
           {:gene :var, :name not}
           {:gene :apply})))

  ;;math with 4+ arguements 
  (is (= (de/decompile-ast (ana.jvm/analyze '(+ 1 2 3 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 1}
           {:gene :var, :name +}
           {:gene :apply}
           {:gene :var, :name +}
           {:gene :apply}
           {:gene :var, :name +}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(* 1 2 3 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 1}
           {:gene :var, :name *}
           {:gene :apply}
           {:gene :var, :name *}
           {:gene :apply}
           {:gene :var, :name *}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(- 1 2 3 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 1}
           {:gene :var, :name -}
           {:gene :apply}
           {:gene :var, :name -}
           {:gene :apply}
           {:gene :var, :name -}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(/ 1 2 3 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 1}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-div}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-div}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-div}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(+ 1.1 2.2 3.3 4.4)))
         '({:gene :lit, :type {:type double?}, :val 4.4}
           {:gene :lit, :type {:type double?}, :val 3.3}
           {:gene :lit, :type {:type double?}, :val 2.2}
           {:gene :lit, :type {:type double?}, :val 1.1}
           {:gene :var, :name +}
           {:gene :apply}
           {:gene :var, :name +}
           {:gene :apply}
           {:gene :var, :name +}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(* 1.1 2.2 3.3 4.4)))
         '({:gene :lit, :type {:type double?}, :val 4.4}
           {:gene :lit, :type {:type double?}, :val 3.3}
           {:gene :lit, :type {:type double?}, :val 2.2}
           {:gene :lit, :type {:type double?}, :val 1.1}
           {:gene :var, :name *}
           {:gene :apply}
           {:gene :var, :name *}
           {:gene :apply}
           {:gene :var, :name *}
           {:gene :apply})))

   ;; other math (sin, cos, tan, etc)
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/sin 0.5)))
                               {:type 'double?})
         0.479425538604203))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/cos 0.5)))
                               {:type 'double?})
         0.8775825618903728))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/tan 0.5)))
                               {:type 'double?})
         0.5463024898437905))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/asin 0.5)))
                               {:type 'double?})
         0.5235987755982989))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/acos 0.5)))
                               {:type 'double?})
         1.0471975511965979))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/atan 0.5)))
                               {:type 'double?})
         0.4636476090008061))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/log10 100.0)))
                               {:type 'double?})
         2.0))

  (is (= (de/decompile-ast (ana.jvm/analyze '(- 1.1 2.2 3.3 4.4)))
         '({:gene :lit, :type {:type double?}, :val 4.4}
           {:gene :lit, :type {:type double?}, :val 3.3}
           {:gene :lit, :type {:type double?}, :val 2.2}
           {:gene :lit, :type {:type double?}, :val 1.1}
           {:gene :var, :name -}
           {:gene :apply}
           {:gene :var, :name -}
           {:gene :apply}
           {:gene :var, :name -}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(/ 1.1 2.2 3.3 4.4)))
         '({:gene :lit, :type {:type double?}, :val 4.4}
           {:gene :lit, :type {:type double?}, :val 3.3}
           {:gene :lit, :type {:type double?}, :val 2.2}
           {:gene :lit, :type {:type double?}, :val 1.1}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-div}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-div}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-div}
           {:gene :apply})))

;;inc and dec
  (is (= (de/decompile-ast (ana.jvm/analyze '(inc 4)))
         '({:gene :lit, :type {:type int?}, :val 4} {:gene :var, :name inc} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(dec 4)))
         '({:gene :lit, :type {:type int?}, :val 4} {:gene :var, :name dec} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(inc 4.2)))
         '({:gene :lit, :type {:type double?}, :val 4.2} {:gene :var, :name inc} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(dec 4.2)))
         '({:gene :lit, :type {:type double?}, :val 4.2} {:gene :var, :name dec} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(abs -10)))
         '({:gene :lit, :type {:type int?}, :val -10} {:gene :var, :name abs} {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(abs -10.5)))
         '({:gene :lit, :type {:type double?}, :val -10.5} {:gene :var, :name abs} {:gene :apply})))

  ;; int and double casting
  (is (= (de/decompile-ast (ana.jvm/analyze '(int 5.5)))
         '({:gene :lit, :type {:type double?}, :val 5.5} {:gene :var, :name int} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(int \t)))
         '({:gene :lit, :type {:type char?}, :val \t} {:gene :var, :name int} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(double 5)))
         '({:gene :lit, :type {:type int?}, :val 5} {:gene :var, :name double} {:gene :apply})))

  ;; Other math
  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/pow 2.0 3.0)))
         '({:gene :lit, :type {:type double?}, :val 3.0}
           {:gene :lit, :type {:type double?}, :val 2.0}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/pow}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/pow 2.5 2.5)))
         '({:gene :lit, :type {:type double?}, :val 2.5}
           {:gene :lit, :type {:type double?}, :val 2.5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/pow}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/ceil 4.5)))
         '({:gene :lit, :type {:type double?}, :val 4.5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/ceil}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/floor 4.5)))
         '({:gene :lit, :type {:type double?}, :val 4.5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/floor}
           {:gene :apply}))))

(deftest decompile-recompile-function-calls-test
  ;; mathematical operations
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 22 33)))
                               {:type 'int?})
         55))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 22 (+ 33 44))))
                               {:type 'int?})
         99))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 22.2 33.3)))
                               {:type 'double?})
         55.5))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 22 33)))
                               {:type 'int?})
         -11))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 22.5 33.5)))
                               {:type 'double?})
         -11.0))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 22 (- 33 44))))
                               {:type 'int?})
         33))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 1)))
                               {:type 'int?})
         -1))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 1.0)))
                               {:type 'double?})
         -1.0))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 4 5 (- 2))))
                               {:type 'int?})
         1))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(* 3 4)))
                               {:type 'int?})
         12))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(* 3.2 4.2)))
                               {:type 'double?})
         13.440000000000001))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(quot 80 7)))
                               {:type 'int?})
         11))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(quot 80.3 7.8)))
                               {:type 'double?})
         10.0))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(/ 3 4))) 
                               {:type 'double?})
         3/4))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(/ 3.5 7.0)))
                               {:type 'double?})
         0.5))

  ;; comparison (<, <=, >, >=)
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(< 4 4)))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(< \a \b)))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(< "hi" "there")))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(<= 4 4)))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(<= \a \a)))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(< "you there" "hi!")))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(> 5 4)))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(> \a \b)))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(> "hi" "there")))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(>= 5 5)))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(>= \a \b)))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(>= "hi" "there")))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast
                                (ana.jvm/analyze '(zero? 0)))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast
                                (ana.jvm/analyze '(zero? (- 4 1))))
                               {:type 'boolean?})
         false))

  ;;max and min
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(max "hi" "yellow" "there")))
                               {:type 'string?})
         "yellow"))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(max \d \f \a)))
                               {:type 'char?})
         \f))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(max 2 98 5)))
                               {:type 'int?})
         98))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(max 9.7 98.3 5.0974)))
                               {:type 'double?})
         98.3))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(min "hi" "yellow" "there")))
                               {:type 'string?})
         "hi"))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(min \d \f \a)))
                               {:type 'char?})
         \a))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(min 2 98 5)))
                               {:type 'int?})
         2))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(min 9.7 98.3 5.0974)))
                               {:type 'double?})
         5.0974))

  ;; not
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(not true)))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(not (< 4 5))))
                               {:type 'boolean?})
         false))

  ;other math (sin, cos, tan, etc)
  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/sin 0.5)))
         '({:gene :lit, :type {:type double?}, :val 0.5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/sin}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/cos 0.5)))
         '({:gene :lit, :type {:type double?}, :val 0.5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/cos}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/tan 0.5)))
         '({:gene :lit, :type {:type double?}, :val 0.5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/tan}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/asin 0.5)))
         '({:gene :lit, :type {:type double?}, :val 0.5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-asin}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/acos 0.5)))
         '({:gene :lit, :type {:type double?}, :val 0.5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-acos}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/atan 0.5)))
         '({:gene :lit, :type {:type double?}, :val 0.5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/atan}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/log10 100)))
         '({:gene :lit, :type {:type int?}, :val 100}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-log10}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/sqrt 16.0)))
         '({:gene :lit, :type {:type double?}, :val 16.0}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-sqrt}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(Math/sqrt -1.0)))
         '({:gene :lit, :type {:type double?}, :val -1.0}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/safe-sqrt}
           {:gene :apply})))

  ;;math with 4+ arguemnts
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 1.1 2.2 3.3 4.4)))
                               {:type 'double?})
         -8.8))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 1.1 2.2 3.3 4.4)))
                               {:type 'double?})
         11.0))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(* 1.1 2.2 3.3 4.4)))
                               {:type 'double?})
         35.138400000000004))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(/ 1.1 2.2 3.3 4.4)))
                               {:type 'double?})
         0.03443526170798898))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(* 1 2 3 4)))
                               {:type 'int?})
         24))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 1 2 3 4)))
                               {:type 'int?})
         -8))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 1 2 3 4)))
                               {:type 'int?})
         10))
  ;;doesn't work
;;   (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(lib/safe-div 1 2 3 4)))
;;                                {:type 'double?})
;;          1/24))

  ;; working through div for more the 2
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(/ 3 4)))
                               {:type 'double?})
         3/4))
;;   (de/compile-debugging '({:gene :lit, :type {:type int?}, :val 3}
;;                         {:gene :lit, :type {:type int?}, :val 2}
;;                         {:gene :lit, :type {:type int?}, :val 1}
;;                         {:gene :var, :name `lib/safe-div}
;;                         {:gene :apply}
;;                         {:gene :var, :name `lib/safe-div}
;;                         {:gene :apply}
;;                         {:gene :var, :name `lib/safe-div}
;;                         {:gene :apply})
;;                       {:type 'int?}) 

  ;;inc and dec
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(dec 5)))
                               {:type 'int?})
         4))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(dec 5.4)))
                               {:type 'double?})
         4.4))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(inc 5)))
                               {:type 'int?})
         6))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(inc 5.4)))
                               {:type 'double?})
         6.4))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(abs -10)))
                               {:type 'int?})
         10))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(abs -10.5)))
                               {:type 'double?})
         10.5))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(int 5.5))) {:type 'int?})
         5))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(int \t))) {:type 'int?})
         116))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(double 5))) {:type 'double?})
         5.0))

;; These only work on doubles 
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/pow 2.0 3.0))) {:type 'double?})
         8.0))
  (is (= (format "%.4f" (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/pow 2.5 2.5))) {:type 'double?}))
         "9.8821"))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/ceil 4.5))) {:type 'double?})
         5.0))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/floor 4.5))) {:type 'double?})
         4.0))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/sqrt 9.0)))
                               {:type 'double?})
         3.0))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/sqrt -64.0)))
                               {:type 'double?})
         8.0))
  (is (= (format "%.5f" (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(Math/sqrt 3.1415927)))
                                              {:type 'double?}))
         "1.77245")))

(deftest decompile-collections-test
  ;; empty collections
  (is (= (de/decompile-ast (ana.jvm/analyze '(empty? [])))
         '({:gene :lit, :type {:child {:sym T, :type :s-var}, :type :vector}, :val []} {:gene :var, :name empty?} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(concat [] [1 2])))
         '({:gene :lit, :type {:child {:type int?}, :type :vector}, :val [1 2]}
           {:gene :lit, :type {:child {:sym T, :type :s-var}, :type :vector}, :val []}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/concat'}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(conj [] 5)))
         '({:gene :lit :val 5 :type {:type int?}}
           {:gene :lit, :type {:child {:sym T, :type :s-var}, :type :vector}, :val []}
           {:gene :var :name erp12.cbgp-lite.lang.lib/conj'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(conj #{} 5)))
         '({:gene :lit :val 5 :type {:type int?}}
           {:gene :lit, :type {:child {:sym T, :type :s-var}, :type :set}, :val #{}}
           {:gene :var :name erp12.cbgp-lite.lang.lib/conj'}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(assoc {} "a" 10)))
         '({:gene :lit, :type {:type int?}, :val 10}
           {:gene :lit, :type {:type string?}, :val "a"}
           {:gene :lit, :type {:key {:sym T, :type :s-var}, :type :map-of, :value {:sym S, :type :s-var}}, :val {}}
           {:gene :var, :name assoc}
           {:gene :apply})))

  ;; vectors
  (is (= (de/decompile-ast (ana.jvm/analyze '(concat [1 2] [3 4])))
         '({:gene :lit, :type {:child {:type int?}, :type :vector}, :val [3 4]}
           {:gene :lit, :type {:child {:type int?}, :type :vector}, :val [1 2]}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/concat'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(concat (rest [1 2 3]) [3 4])))
         '({:gene :lit, :type {:child {:type int?}, :type :vector}, :val [3 4]}
           {:gene :lit, :type {:child {:type int?}, :type :vector}, :val [1 2 3]}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/rest'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/concat'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(last [1 2 3])))
         '({:gene :lit, :type {:child {:type int?}, :type :vector}, :val [1 2 3]} {:gene :var, :name last} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(first [1 2 3])))
         '({:gene :lit, :type {:child {:type int?}, :type :vector}, :val [1 2 3]} {:gene :var, :name first} {:gene :apply})))

  ;; strings
  (is (= (de/decompile-ast (ana.jvm/analyze '(first "Hello")))
         '({:gene :lit, :type {:type string?}, :val "Hello"} {:gene :var, :name first} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(last "String")))
         '({:gene :lit, :type {:type string?}, :val "String"} {:gene :var, :name last} {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(count "Hamilton")))
         '({:gene :lit, :type {:type string?}, :val "Hamilton"}
           {:gene :var, :name count}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(count [1 2 3 4])))
         '({:gene :lit,
            :type {:child {:type int?}, :type :vector}, :val [1 2 3 4]}
           {:gene :var, :name count}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(count #{1.1 2.2 3.3 4.4})))
         '({:gene :lit,
            :type {:child {:type double?}, :type :set}, :val #{1.1 2.2 3.3 4.4}}
           {:gene :var, :name count}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(count {"a" 1 "b" 2 "c" 3})))
         '({:gene :lit,
            :type {:key {:type string?}, :type :map-of, :value {:type int?}}, :val {"a" 1 "b" 2 "c" 3}}
           {:gene :var, :name count}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(vals {1 \a 2 \b})))
         '({:gene :lit, :type {:key {:type int?}, :type :map-of, :value {:type char?}}, :val {1 \a, 2 \b}}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/vals-vec}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(keys {1 #{1 2} 2 #{3 4}})))
         '({:gene :lit,
            :type {:key {:type int?}, :type :map-of, :value {:child {:type int?}, :type :set}},
            :val {1 #{1 2}, 2 #{3 4}}}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/keys-vec}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(merge {"a" 1} {"b" 2})))
         '({:gene :lit, :type {:key {:type string?}, :type :map-of, :value {:type int?}}, :val {"b" 2}}
           {:gene :lit, :type {:key {:type string?}, :type :map-of, :value {:type int?}}, :val {"a" 1}}
           {:gene :var, :name merge}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(merge {[\a \a \a] 1} {[\b \b \b] 2})))
         '({:gene :lit,
            :type {:key {:child {:type char?}, :type :vector}, :type :map-of, :value {:type int?}},
            :val {[\b \b \b] 2}}
           {:gene :lit,
            :type {:key {:child {:type char?}, :type :vector}, :type :map-of, :value {:type int?}},
            :val {[\a \a \a] 1}}
           {:gene :var, :name merge}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(str "hello" "world")))
         '({:gene :lit, :type {:type string?}, :val "world"}
           {:gene :lit, :type {:type string?}, :val "hello"}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/concat'}
           {:gene :apply})))
  ; doesn't work; 3+ args
  #_(is (= (de/decompile-ast (ana.jvm/analyze '(str "hello" "world" "yay")))
           '({:gene :lit, :type {:type string?}, :val "yay"}
             {:gene :lit, :type {:type string?}, :val "world"}
             {:gene :lit, :type {:type string?}, :val "hello"}
             {:gene :var, :name erp12.cbgp-lite.lang.lib/concat'}
             {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(str (+ 2 3))))
         '({:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :var, :name +}
           {:gene :apply}
           {:gene :var, :name str}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(nth [1.0 2.0 3.0] 10 4.04)))
         '({:gene :lit, :type {:type double?}, :val 4.04}
           {:gene :lit, :type {:type int?}, :val 10}
           {:gene :lit, :type {:child {:type double?}, :type :vector}, :val [1.0 2.0 3.0]}
           {:gene :var, :name nth-or-else}
           {:gene :apply})))

;; partial and comp
  (is (= (de/decompile-ast (ana.jvm/analyze '((partial * 100) 5)))
         '({:gene :lit, :type {:type int?}, :val 5}
           {:gene :lit, :type {:type int?}, :val 100}
           {:gene :var, :name *}
           {:gene :var, :name partial}
           {:gene :apply}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '((partial assoc {\a 1 \b 2 \c 3}) \z 42)))
         '({:gene :lit, :type {:type int?}, :val 42}
           {:gene :lit :val \z :type {:type char?}}
           {:gene :lit :val {\a 1 \b 2 \c 3} :type {:type :map-of :key {:type char?} :value {:type int?}}}
           {:gene :var :name assoc}
           {:gene :var :name partial}
           {:gene :apply}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '((partial assoc {\a 1 \b 2 \c 3} \z) 42)))
         '({:gene :lit, :type {:type int?}, :val 42}
           {:gene :lit :val \z :type {:type char?}}
           {:gene :lit :val {\a 1 \b 2 \c 3} :type {:type :map-of :key {:type char?} :value {:type int?}}}
           {:gene :var :name assoc}
           {:gene :var :name partial}
           {:gene :apply}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '((partial assoc {0 42 1 2999 2 108} 3) 42)))
         '({:gene :lit, :type {:type int?}, :val 42}
           {:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit :val {0 42 1 2999 2 108} :type {:type :map-of :key {:type int?} :value {:type int?}}}
           {:gene :var :name assoc}
           {:gene :var :name partial}
           {:gene :apply}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '((comp dec +) 5 100)))
         '({:gene :lit, :type {:type int?}, :val 100}
           {:gene :lit, :type {:type int?}, :val 5}
           {:gene :var :name +}
           {:gene :var :name dec}
           {:gene :var :name comp}
           {:gene :apply}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '((comp dec dec +) 100 45)))
         '({:gene :lit, :type {:type int?}, :val 45}
           {:gene :lit, :type {:type int?}, :val 100} 
           {:gene :var :name +}
           {:gene :var :name dec}
           {:gene :var :name dec}
           {:gene :var :name comp}
           {:gene :apply}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '((comp + -) 5 65 100)))
         '({:gene :lit, :type {:type int?}, :val 100}
           {:gene :lit, :type {:type int?}, :val 65}
           {:gene :lit, :type {:type int?}, :val 5}
           {:gene :var :name -}
           {:gene :var :name +}
           {:gene :var :name comp}
           {:gene :apply}
           {:gene :apply}))))

(deftest decompile-recompile-collections-test
  ;; empty collections 
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(empty? [])))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(concat [] [1 2])))
                               {:type :vector :child {:type 'int?}})
         [1 2]))

  ; replace concat'd genome w/ (ana.jvm/analyze '(conj [] 5)) when conj-vec is added
  ; (is (= (de/compile-debugging (concat (de/decompile-ast (ana.jvm/analyze '(conj [] 5))
  (is (= (de/compile-debugging (concat (de/decompile-ast (ana.jvm/analyze []))
                                       (list {:gene :var :name `lib/conj'}
                                             {:gene :lit :val 5 :type {:type 'int?}}
                                             {:gene :apply}))
                               {:type :vector :child {:type 'int?}})
         [5]))

  ; replace concat'd genome w/ (ana.jvm/analyze '(conj #{} 5)) when conj-set is added
  ; (is (= (de/compile-debugging (concat (de/decompile-ast (ana.jvm/analyze '(conj #{} 5))
  (is (= (de/compile-debugging (concat (de/decompile-ast (ana.jvm/analyze #{}))
                                       (list {:gene :var :name `lib/conj'}
                                             {:gene :lit :val 5 :type {:type 'int?}}
                                             {:gene :apply}))
                               {:type :set :child {:type 'int?}})
         #{5}))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(assoc {} "a" 10)))
                               {:key {:type 'string?}, :type :map-of, :value {:type 'int?}})
         {"a" 10}))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(assoc {"b" 42} "a" 10)))
                               {:key {:type 'string?}, :type :map-of, :value {:type 'int?}})
         {"b" 42 "a" 10}))

  ;; vectors
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(concat [1 2] [3 4])))
                               {:child {:type 'int?} :type :vector})
         [1 2 3 4]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(concat (rest [1 2 3]) [3 4])))
                               {:child {:type 'int?} :type :vector})
         [2 3 3 4]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(rest [1 2 3 4])))
                               {:type :vector :child {:type 'int?}})
         [2 3 4]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(first [1.1 2.2 3.3])))
                               {:type 'double?})
         1.1))

  ;; strings
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(first "Hello")))
                               {:type 'char?})
         \H))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(empty? ["Hi" "Hello" "Hey"])))
                               {:type 'boolean?})
         false))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(empty? "")))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(last [\C \a \t]))) {:type 'char?})
         \t))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(last "String")))
                               {:type 'char?})
         \g))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(rest "String")))
                               {:type 'string?})
         "tring"))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(count "Hamilton")))
                               {:type 'int?})
         8))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(rest [1 2 3 4])))
                               {:type :vector :child {:type 'int?}})
         [2 3 4]))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(count [1 2 3 4])))
                               {:type 'int?})
         4))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(count #{1.1 2.2 3.3})))
                               {:type 'int?})
         3))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(count {"a" 1 "b" 2 "c" 3})))
                               {:type 'int?})
         3))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(vals {1 #{1 2} 2 #{3 4}})))
                               {:type :vector :child {:child {:type 'int?}, :type :set}})
         [#{1 2} #{3 4}]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(keys {1 #{1 2} 2 #{3 4}})))
                               {:type :vector :child {:type 'int?}})
         [1 2]))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(merge {"a" 1} {"b" 2})))
                               '{:type :map-of :key {:type string?} :value {:type int?}})
         {"a" 1 "b" 2}))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(str "hello" "world")))
                               {:type 'string?})
         "helloworld"))
  ; doesn't work; 3+ args
  #_(is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(str "hello" "world" "yay")))
                                 {:type 'string?})
           "helloworldyay"))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(str (+ 2 3))))
                               {:type 'string?})
         "5"))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(nth ["Hi" "Hey" "Hello"] 1)))
                               {:type 'string?})
         "Hey"))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(nth [1 2 3 4] 5 404)))
                               {:type 'int?})
         404))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(nth [1.0 2.0 3.0 5.0] 3 4.04)))
                               {:type 'double?})
         5.0))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(vec (remove #(= % \!) "hi!")))) 
                               {:type :vector :child {:type 'char?}})
         [\h \i]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(remove #(zero? %) [0 2 3 3 0]))) 
                               {:type :vector :child {:type 'int?}})
         [2 3 3])))

(deftest decompile-misc-test
  ; if
  (is (= (de/decompile-ast (ana.jvm/analyze '(if false 5 2)))
         '({:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 5}
           {:gene :lit, :type {:type boolean?}, :val false}
           {:gene :var, :name if}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(if (= 1 2) (max 10 11) 12)))
         '({:gene :lit, :type {:type int?}, :val 12}
           {:gene :lit, :type {:type int?}, :val 11}
           {:gene :lit, :type {:type int?}, :val 10}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
           {:gene :apply}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 1}
           {:gene :var, :name =}
           {:gene :apply}
           {:gene :var, :name if}
           {:gene :apply})))

  ; printing
  (is (= (de/decompile-ast (ana.jvm/analyze '(print "hi")))
         '({:gene :lit, :type {:type string?}, :val "hi"} {:gene :var, :name print} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(println "hi")))
         '({:gene :lit, :type {:type string?}, :val "hi"} {:gene :var, :name println} {:gene :apply}))))

(deftest decompile-recompile-misc-test
  ; if
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(if false 5 2)))
                               {:type 'int?})
         2))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(if (= 1 2) (max 10 11) 12)))
                               {:type 'int?})
         12))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(if (= 0 1) (= 1 10) (< 1 2))))
                               {:type 'boolean?})
         true))
  ; multitype test?

  ; printing 
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(print "hi")))
                               {:type 'nil?})
         nil))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(println "hi")))
                               {:type 'nil?})
         nil)))

(deftest decompile-locals-collection-test
  ;; first w/ locals
  (is (= (de/decompile-ast (ana.jvm/analyze '(defn my-first [input1] (first input1)))
                           {:input->type {'input1 {:type 'string?}}
                            :ret-type {:type 'char?}})
         '({:gene :local, :idx 0} {:gene :var, :name first} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(defn my-first [input1] (first input1)))
                           {:input->type {'input1 {:type :vector :child {:type 'int?}}}
                            :ret-type {:type 'int?}})
         '({:gene :local, :idx 0} {:gene :var, :name first} {:gene :apply})))

  ;; last w/ locals
  (is (= (de/decompile-ast (ana.jvm/analyze '(defn my-first [input1] (last input1)))
                           {:input->type {'input1 {:type 'string?}}
                            :ret-type {:type 'char?}})
         '({:gene :local, :idx 0} {:gene :var, :name last} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(defn my-first [input1] (last input1)))
                           {:input->type {'input1 {:type :vector :child {:type 'int?}}}
                            :ret-type {:type 'int?}})
         '({:gene :local, :idx 0} {:gene :var, :name last} {:gene :apply}))))

(deftest decompile-recompile-locals-collection-test
  ;; first w/ locals
  (is (= (de/compile-debugging2 (de/decompile-ast (ana.jvm/analyze '(defn my-first [input1] (first input1)))
                                                  {:input->type {'input1 {:type 'string?}}
                                                   :ret-type {:type 'char?}})
                                {:input->type {'input1 {:type 'string?}}
                                 :ret-type {:type 'char?}}
                                ["Hello"])
         \H))
  (is (= (de/compile-debugging2 (de/decompile-ast (ana.jvm/analyze '(defn my-first [input1] (first input1)))
                                                  {:input->type {'input1 {:type :vector :child {:type 'double?}}}
                                                   :ret-type {:type 'int?}})
                                {:input->type {'input1 {:type :vector :child {:type 'double?}}}
                                 :ret-type {:type 'double?}}
                                [[1.6 3.2 6.4 12.8]])
         1.6))

  ;; last w/ locals
  (is (= (de/compile-debugging2 (de/decompile-ast (ana.jvm/analyze '(defn my-first [input1] (last input1)))
                                                  {:input->type {'input1 {:type 'string?}}
                                                   :ret-type {:type 'char?}})
                                {:input->type {'input1 {:type 'string?}}
                                 :ret-type {:type 'char?}}
                                ["Hello"])
         \o))
  (is (= (de/compile-debugging2 (de/decompile-ast (ana.jvm/analyze '(defn my-first [input1] (last input1)))
                                                  {:input->type {'input1 {:type :vector :child {:type 'double?}}}
                                                   :ret-type {:type 'int?}})
                                {:input->type {'input1 {:type :vector :child {:type 'double?}}}
                                 :ret-type {:type 'double?}}
                                [[1.6 3.2 6.4 12.8]])
         12.8)))

(deftest decompile-locals-test
  (is (= (de/decompile-ast (ana.jvm/analyze '(defn local_int [input1] (inc input1)))
                           {:input->type {'input1 {:type 'int?}}
                            :ret-type {:type 'int?}})
         '({:gene :local, :idx 0} {:gene :var, :name inc} {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(defn local_double [input1] (- input1)))
                           {:input->type {'input1 {:type 'double?}}
                            :ret-type {:type 'double?}})
         '({:gene :local, :idx 0} {:gene :var, :name erp12.cbgp-lite.lang.lib/neg} {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(defn local_strint [input1] (count input1)))
                           {:input->type {'input1 {:type 'string?}}
                            :ret-type {:type 'int?}})
         '({:gene :local, :idx 0} {:gene :var, :name count} {:gene :apply})))
  ;; broken until local var map is fixed
  (is (= (de/decompile-ast (ana.jvm/analyze '(defn help [input1 input2] (+ input1 input2)))
                           {:input->type {'input1 {:type 'int?}
                                          'input2 {:type 'int?}}
                            :ret-type {:type 'int?}})
         '({:gene :local, :idx 1} {:gene :local, :idx 0} {:gene :var, :name +} {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(defn help [input1 input2] (+ (int input1) (int input2))))
                           {:input->type {'input1 {:type 'double?}
                                          'input2 {:type 'double?}}
                            :ret-type {:type 'int?}})
         '({:gene :local, :idx 1}
           {:gene :var, :name int}
           {:gene :apply}
           {:gene :local, :idx 0}
           {:gene :var, :name int}
           {:gene :apply}
           {:gene :var, :name +}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(defn help [input1 input2] (+ input2 (int input1))))
                           {:input->type {'input1 {:type 'double?}
                                          'input2 {:type 'int?}}
                            :ret-type {:type 'int?}})
         '({:gene :local, :idx 1}
           {:gene :var, :name int}
           {:gene :apply}
           {:gene :local, :idx 0}
           {:gene :var, :name +}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(defn help [input1 input2] (+ (count input1) input2)))
                           {:input->type {'input1 {:type 'string?}
                                          'input2 {:type 'int?}}
                            :ret-type {:type 'int?}})
         '({:gene :local, :idx 1}
           {:gene :local, :idx 0}
           {:gene :var, :name count}
           {:gene :apply}
           {:gene :var, :name +}
           {:gene :apply}))))

(deftest decompile-recompile-locals-test
  (is (= (de/compile-debugging2 (de/decompile-ast (ana.jvm/analyze '(defn local_int [input1] (inc input1)))
                                                  {:input->type {'input1 {:type 'int?}}
                                                   :ret-type {:type 'int?}})
                                {:input->type {'input1 {:type 'int?}}
                                 :ret-type {:type 'int?}}
                                [1])
         2))

  (is (= (de/compile-debugging2 (de/decompile-ast (ana.jvm/analyze '(defn local_double [input1] (- input1)))
                                                  {:input->type {'input1 {:type 'double?}}
                                                   :ret-type {:type 'double?}})
                                {:input->type {'input1 {:type 'double?}}
                                 :ret-type {:type 'double?}}
                                [1.5])
         -1.5))
  (is (= (de/compile-debugging2 (de/decompile-ast (ana.jvm/analyze '(defn local_strint [input1] (count input1)))
                                                  {:input->type {'input1 {:type 'string?}}
                                                   :ret-type {:type 'int?}})
                                {:input->type {'input1 {:type 'string?}}
                                 :ret-type {:type 'int?}}
                                ["hello"])
         5))

  (is (= (de/compile-debugging2 (de/decompile-ast (ana.jvm/analyze '(defn help [input1 input2] (+ input1 input2)))
                                                  {:input->type {'input1 {:type 'int?}
                                                                 'input2 {:type 'int?}}
                                                   :ret-type {:type 'int?}})
                                {:input->type {'input1 {:type 'int?}
                                               'input2 {:type 'int?}}
                                 :ret-type {:type 'int?}}
                                [9 10])
         19))

  (is (= (de/compile-debugging2 (de/decompile-ast (ana.jvm/analyze '(defn help [input1 input2] (+ (int input1) (int input2))))
                                                  {:input->type {'input1 {:type 'double?}
                                                                 'input2 {:type 'double?}}
                                                   :ret-type {:type 'int?}})
                                {:input->type {'input1 {:type 'double?}
                                               'input2 {:type 'double?}}
                                 :ret-type {:type 'int?}}
                                [9.0 10.0])
         19))

  (is (= (de/compile-debugging2 (de/decompile-ast (ana.jvm/analyze '(defn help [input1 input2] (+ (int input1) input2))))
                                {:input->type {'input1 {:type 'double?}
                                               'input2 {:type 'int?}}
                                 :ret-type {:type 'int?}}
                                [9.0 10])
         19))

  (is (= (de/compile-debugging2 (de/decompile-ast (ana.jvm/analyze '(defn help [input1 input2] (+ (count input1) input2)))
                                                  {:input->type {'input1 {:type 'string?}
                                                                 'input2 {:type 'int?}}
                                                   :ret-type {:type 'int?}})
                                {:input->type {'input1 {:type 'string?}
                                               'input2 {:type 'int?}}
                                 :ret-type {:type 'int?}}
                                ["hello" 10])
         15)))
  

(deftest compile-partial-and-comp
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((partial assoc {\a 1 \b 2 \c 3}) \z 42)))
                               {:type :map-of, :key {:type 'char?}, :value {:type 'int?}})
         {\a 1, \b 2, \c 3, \z 42}))
  
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((partial assoc {0 42 1 2999 2 108} 3) 42)))
                               {:type :map-of, :key {:type 'int?}, :value {:type 'int?}})
         {0 42, 1 2999, 2 108, 3 42}))
  
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((partial assoc {\a 1 \b 2 \c 3} \z) 42)))
                               {:type :map-of, :key {:type 'char?}, :value {:type 'int?}})
         {\a 1, \b 2, \c 3, \z 42}))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((partial * 100) 5)))
                               {:type 'int?})
         500))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((comp dec +) 5 100)))
                               {:type 'int?})
         104)) 
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((comp dec dec +) 5 100)))
                               {:type 'int?})
         103)) 
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((comp + -) 5 65 100)))
                               {:type 'int?})
         -30))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((comp str dec +) 5 65)))
                               {:type 'string?})
         "69")))

  ;; macros

;; let
(deftest decompile-recompile-let-tests
  (testing "simple lets"
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 2 y 6 v 56] (+ y x))))
                                 {:type 'int?})
           8))

    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 2 y 6 v 56] (+ y v x))))
                                 {:type 'int?})
           64))

    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 2 y 6] (+ y x))))
                                 {:type 'int?})
           8))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 1
                                                                           y (+ 2 3)]
                                                                       (+ x y))))
                                 {:type 'int?})
           6)))

  (testing "anonymous function lets"
    ; works
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 4
                                                                           y (fn [z] (* z 8))]
                                                                       (y x))))
                                 {:type 'int?})
           32))
           32))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x [2 3]
                                                                           y (fn [z] (mapv inc z))]
                                                                       (y x))))
                                 {:type :vector :child {:type 'int?}})
           [3 4]))

    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(mapv (fn [z]
                                                                             (+ 2 z))
                                                                           [2 3])))
                                 {:type :vector :child {:type 'int?}})
           [4 5]))

    ;; multi-fn series
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x (remove (fn [test] (zero? test)) [0 1 3 2 1 1])
                                                                           y (fn [z] (* z 8))]
                                                                       (mapv y x))))
                                 {:type :vector :child {:type 'int?}})
           [8 24 16 8 8]))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x (count (remove (fn [x2] (zero? x2)) [0 1 3 2 1 1 1]))
                                                                           y (count (remove (fn [y2] (zero? y2)) [0 0 1 2 3 0 0]))]
                                                                       (+ x y))))
                                 {:type 'int?})
           9))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 4
                                                                           y (remove (fn [x2] (zero? x2)) [0 1 3 2 1 1])]
                                                                       (+ (count y) x))))
                                 {:type 'int?})
           9))

    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 4
                                                                           y (count (remove (fn [y2] (zero? y2)) [0 0 1 2 3]))]
                                                                       (+ y x))))
                                 {:type 'int?})
           7))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 4
                                                                           y (fn [y2] (remove (fn [y3] (zero? y3)) y2))]
                                                                       (+ (count (y [0 1 3 2 1 1 0 0])) x))))
                                 {:type 'int?})
           9))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 4
                                                                           y ((fn [z] (+ 2 z)) 3)]
                                                                       (+ y x))))
                                 {:type 'int?})
           9))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 4
                                                                           y (fn [z] (+ 2 z))]
                                                                       (+ (y 3) x))))
                                 {:type 'int?})
           9))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 4
                                                                           y ((fn [z] (+ 2 z)) 3)]
                                                                       (+ y x))))
                                 {:type 'int?})
           9))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [y ((fn [z] (+ 2 z)) 3)
                                                                           x 4]
                                                                       (+ y x))))
                                 {:type 'int?})
           9))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 4
                                                                           w 3
                                                                           y ((fn [z] (+ 2 z)) w)]
                                                                       (+ y x))))
                                 {:type 'int?})
           9))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x (remove (fn [x2] (zero? x2)) [0 1 3 2 1 1 0 0])
                                                                           y 4]
                                                                       (+ (count x) y))))
                                 {:type 'int?})
           9)))

  (testing "nested anonymous function lets"
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x [3 2 1]
                                                                           y (fn [z] (* z 8))]

                                                                       (mapv y x))))
                                 {:type :vector :child {:type 'int?}})
           [24 16 8])) 
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x (remove (fn [test] (zero? test)) [0 0 2 3])
                                                                           y (fn [z] (mapv #(* % 8) z))]
                                                                       (y x))))
                                 {:type :vector :child {:type 'int?}})
           [16 24])) 
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x (fn [z] (mapv #(* % 8) z))
                                                                           y [2 3]]
                                                                       (x y))))
                                 {:type :vector :child {:type 'int?}})
           [16 24]))))
    
(deftest decompile-anonymous-functions-test
  ; broken b/c mismatched s-var names
  #_(is (partial= (de/decompile-ast (ana.jvm/analyze '(remove #(zero? %) [0 2 3 3 0])))
         '({:gene :lit, :type {:child {:type int?}, :type :vector}, :val [0 2 3 3 0]}
           {:arg-types [{:sym T, :type :s-var}], :gene :fn, :ret-type {:type boolean?}}
           {:gene :local, :idx 0}
           {:gene :var, :name zero?}
           {:gene :apply}
           {:gene :close}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/remove'}
           {:gene :apply})))
  #_(is (partial= (de/decompile-ast (ana.jvm/analyze '(fn [x] (+ x 1))))
         '({:arg-types [{:sym T, :type :s-var}], :gene :fn, :ret-type {:sym T, :type :s-var}}
           ({:gene :lit, :type {:type int?}, :val 1} {:gene :local, :idx 0} {:gene :var, :name +} {:gene :apply})
           {:gene :close})))
  #_(is (=  '({:gene :lit, :type {:child {:type int?}, :type :vector}, :val [0 1 2]} 
              {:arg-types [{:sym T, :type :s-var}], :gene :fn, :ret-type {:sym T, :type :s-var}}
              {:gene :lit, :type {:type int?}, :val 1}
              {:gene :local, :idx 0}
              {:gene :var, :name +}
              {:gene :apply}
              {:gene :close}
              {:gene :var, :name mapv}
              {:gene :apply}
              {:gene :apply})
          (de/decompile-ast (ana.jvm/analyze '((mapv (fn [x] (+ x 1)) [0 1 2]))))))) 
    
(deftest decompile-recompile-anonymous-functions-test
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(remove #(zero? %) [0 2 3 3 0])))
                               {:type :vector :child {:type 'int?}})
         [2 3 3]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((mapv (fn [x] (+ x 1)) [0 1 2]))))
                               {:type :vector :child {:type 'int?}})
         [1 2 3]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x (fn [y] (+ 2 y))] (x 5)))) {:type 'int?})
         7))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [y (remove (fn [y2] (zero? y2)) [0 1 3 2 1 1])
                                                                         z 6
                                                                         x 4]
                                                                     (+ (count y) (- z x)))))
                               {:type 'int?})
         7))
  
  (testing "nested let-fn"
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [a (fn [x] (+ x 2))
                                                                           b (fn [y z] (* y z))
                                                                           c 5]
                                                                       (a (b c 10))))) {:type 'int?})
           52))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 4
                                                                           a (fn [x] (+ 2 x))
                                                                           b (fn [y z] (- y z))
                                                                           c (a 8)]
                                                                       (a (b c 1))))) {:type 'int?})
           11))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 4
                                                                           y ((fn [z] (+ 2 z)) 3)]
                                                                       (+ y x)))) {:type 'int?})
           9))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [x 4
                                                                           y (fn [z] (- z 2))
                                                                           z (y x)]
                                                                       (* z (+ x (y 5)))))) {:type 'int?}) ; = 14 
           14))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(let [a (fn [x] (+ 2 x))
                                                                           c (a 8)]
                                                                       (a c)))) {:type 'int?})
           12)))

  (testing "nested fn-let-fn"
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((fn [a b]
                                                                  (let [x (#(* % 2) a)]
                                                                    (+ x b))) -1 5))) {:type 'int?})
           3))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((fn [a b]
                                                                  (let [x (#(* % 2) a)
                                                                        y (#(+ % 10) b)]
                                                                    (+ x y))) -1 5))) {:type 'int?})
           13))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((fn [a b c]
                                                                  (let [x (#(* % 2) a)
                                                                        y (#(+ % 10) b)
                                                                        z (#(- % 2) c)]
                                                                    (+ x y z))) -1 10 9))) {:type 'int?})
           25))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((fn [a]
                                                                  (let [x (#(* % 2) a)]
                                                                    (inc x))) 8))) {:type 'int?}) 
           17))
    (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '((fn [a b c]
                                                                  (let [x ((fn [x2] (* x2 2)) a)
                                                                        y (fn [z] (mapv inc ((fn [z3] (conj z3 4)) ((fn [z2] (remove zero? z2)) z))))
                                                                        w ((fn [w2] (- w2 -5)) b)]
                                                                    (+ x (count (y [3 0 2 0 1 0])) w))) -1 10 7))) {:type 'int?})
             17))))
    
