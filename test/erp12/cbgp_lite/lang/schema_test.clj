(ns erp12.cbgp-lite.lang.schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.schema :as sch]
            [clojure.string :as str]))

(deftest occurs?-test
  (is (sch/occurs? 'a {:op :local :name 'a}))
  (is (sch/occurs? {:type :s-var :sym 'A}
                   {:type   :=>
                    :input  {:type     :cat
                             :children [{:type :s-var :sym 'A}]}
                    :output {:type 'int?}}))
  (is (not (sch/occurs? 'x {:op :var :var '+})))
  (is (not (sch/occurs? {:type :s-var :sym 'A}
                        {:type   :=>
                         :input  {:type     :cat
                                  :children [{:type :s-var :sym 'B}]}
                         :output {:type 'int?}}))))

(deftest decompose-typeclass-test
  (testing "Number"
    (is (= #{#{'int? 'double?} :cat :s-var :=> :scheme} (set (sch/decompose-typeclass #{#{:number} :cat :s-var :=> :scheme})))))
  (testing "Comparable"
    (is (= #{#{'int? 'double? 'char? 'string? 'boolean?} :cat :s-var :=> :scheme} (set(sch/decompose-typeclass #{#{:comparable} :cat :s-var :=> :scheme})))))
  (testing "Countable"
    (is (= #{#{:vector :map-of :set 'string?} :cat :s-var :=> :scheme} (set (sch/decompose-typeclass #{#{:countable} :cat :s-var :=> :scheme})))))
  (testing "Indexable"
    (is (= #{#{:vector 'string?} :cat :s-var :=> :scheme} (set (sch/decompose-typeclass #{#{:indexable} :cat :s-var :=> :scheme})))))
  (testing "Intable"
    (is (= #{#{'double? 'char?} :cat :s-var :=> :scheme} (set (sch/decompose-typeclass #{#{:intable} :cat :s-var :=> :scheme})))))
  (testing "Keyable"
    (is (= #{#{:set :map-of} :cat :s-var :=> :scheme} (set (sch/decompose-typeclass #{#{:keyable} :cat :s-var :=> :scheme})))))
  (testing "Stringable"
    (is (= #{#{'string? 'char?} :cat :s-var :=> :scheme} (set (sch/decompose-typeclass #{#{:stringable} :cat :s-var :=> :scheme}))))))

(deftest schema-terms-test
  (testing "No Typeclasses"
    (is (= #{:scheme :cat :=> :map-of :s-var} 
           (sch/schema-terms (get lib/type-env 'get))))
    (is (= #{:cat :=> 'char? 'boolean?} 
           (sch/schema-terms (get lib/type-env `lib/digit?)))))
  (testing "Typeclasses"
    ;; Number
    (is (= #{#{'int? 'double?} :cat :s-var :=> :scheme} 
           (sch/schema-terms (get lib/type-env '+))))
    ;; Comparable
    (is (= #{#{'int? 'double? 'char? 'string? 'boolean?} 'boolean? :cat :s-var :=> :scheme}
           (sch/schema-terms (get lib/type-env `lib/<'))))
    ;; Countable
    (is (= #{#{:vector :map-of :set 'string?} :cat :s-var :=> :scheme 'int?} 
           (sch/schema-terms (get lib/type-env 'count))))
    ;; Indexable
    (is (= #{#{:vector 'string?} 'int? :cat :s-var :=> :scheme}
           (sch/schema-terms (get lib/type-env `lib/index-of))))
    ;; Intable
    (is (= #{#{'double? 'char?} 'int? :cat :s-var :=> :scheme}
           (sch/schema-terms (get lib/type-env 'int))))
    ;; Keyable
    (is (= #{#{:set :map-of} 'boolean? :cat :s-var :=> :scheme}
           (sch/schema-terms (get lib/type-env 'contains?))))
    ;; Stringable
    (is (= #{#{'char? 'string?} :vector 'string? :cat :s-var :=> :scheme}
           (sch/schema-terms (get lib/type-env `str/join)))))
  
  (testing "Overloaded"
    ;; First, in lib order
    (is (= #{:=> :cat :s-var :scheme :vector}
           (sch/schema-terms (first (:alternatives (get lib/type-env 'first))))))
    (is (= #{:cat :=> 'char? 'string?}
           (sch/schema-terms (second (:alternatives (get lib/type-env 'first))))))
    
    ;; in?, in lib order
    (is (= #{:=> :cat :s-var :scheme :vector 'boolean?}
           (sch/schema-terms (first (:alternatives (get lib/type-env `lib/in?))))))
    (is (= #{:=> :cat 'boolean? 'char? 'string?}
           (sch/schema-terms (second (:alternatives (get lib/type-env `lib/in?))))))
    (is (= #{:=> :cat 'boolean? 'string?}
           (sch/schema-terms (last (:alternatives (get lib/type-env `lib/in?)))))) 
    
    ;; Reduce, in lib order
    (is (= #{:=> :cat :s-var :scheme :vector}
           (sch/schema-terms (first (:alternatives (get lib/type-env 'reduce))))))
    (is (= #{:=> :cat :s-var :scheme :set}
           (sch/schema-terms (second (:alternatives (get lib/type-env 'reduce))))))
    (is (= #{:=> :cat :s-var :scheme :map-of :tuple}
           (sch/schema-terms (last (:alternatives (get lib/type-env 'reduce))))))
    ))
