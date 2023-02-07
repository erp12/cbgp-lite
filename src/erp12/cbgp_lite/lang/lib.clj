(ns erp12.cbgp-lite.lang.lib
  (:refer-clojure :exclude [and or vector-of])
  (:require [clojure.core :as core]
            [clojure.set :as set]
            [clojure.string :as str]
            [erp12.cbgp-lite.lang.schema :as schema]))

;; @todo What do do about nil?
;; first, last, etc. return nil on empty collections.
;; inc, +, etc. throw on nil.

(defn and
  [a b]
  ;; Wrap th macro
  (core/and a b))

(defn or
  [a b]
  ;; Wrap the macro
  (core/or a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic Comparisons

(defn <'
  [a b]
  (< (compare a b) 0))

(defn <='
  [a b]
  (core/or (= a b) (<' a b)))

(defn >'
  [a b]
  (not (<=' a b)))

(defn >='
  [a b]
  (not (<' a b)))

(defn min'
  [a b]
  (if (<' a b) a b))

(defn max'
  [a b]
  (if (<' a b) b a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math

(defn safe-div
  [n d]
  (if (zero? d) 0 (/ n d)))

(defn safe-mod
  [n d]
  (if (zero? d) 0 (mod n d)))

(defn safe-quot
  [n d]
  (if (zero? d) 0 (quot n d)))

(defn sin
  [x]
  (Math/sin x))

(defn cos
  [x]
  (Math/cos x))

(defn tan
  [x]
  (Math/tan x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text

(defn int->char
  [i]
  (char (mod i 128)))

(def concat-str (comp str/join concat))
(def take-str (comp str/join take))
(def rest-str (comp str/join rest))
(def butlast-str (comp str/join butlast))
(def filter-str (comp str/join filter))

(defn char-in? [s c] (str/includes? s (str c)))

(defn char-occurrences
  [s c]
  (count (filter #{c} s)))

(def ^:private regex-char-esc-smap
  (let [esc-chars "()*&^%$#!"]
    (zipmap esc-chars
            (map #(str "\\" %) esc-chars))))

(defn- str-to-pattern
  [string]
  (->> string
       str
       (replace regex-char-esc-smap)
       str/join
       re-pattern))

(defn split-str
  [s on]
  (str/split s (str-to-pattern on)))

(defn split-str-on-ws
  [s]
  (str/split (str/trim s) #"\s+"))

(defn replace-char
  [s c1 c2]
  (str/replace s (str c1) (str c2)))

(defn replace-first-char
  [s c1 c2]
  (str/replace-first s (str c1) (str c2)))

(defn remove-char
  [s c]
  (apply str (remove #{c} s)))

(defn set-char
  [s idx c]
  (if (empty? s)
    s
    (let [safe-idx (mod idx (count s))]
      (apply str (assoc (vec s) safe-idx c)))))

(defn safe-subs
  [s start end]
  (let [start (min (count s) (max 0 start))
        end (min (count s) (max start end))]
    (subs s start end)))

(defn whitespace?
  [^Character c]
  (Character/isWhitespace c))

(defn digit?
  [^Character c]
  (Character/isDigit c))

(defn letter?
  [^Character c]
  (Character/isLetter c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coll

(defn safe-nth
  [coll idx]
  (if (empty? coll)
    (throw (ex-info "Cannot take safe-nth of empty vector." {:coll coll :idx idx}))
    (let [idx (mod idx (count coll))]
      (nth coll idx))))

(defn occurrences-of
  [coll el]
  (count (filter #{el} coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vector

(def distinctv (comp vec distinct))
(def mapcatv (comp vec mapcat))
(def mapv-indexed (comp vec map-indexed))
(def removev (comp vec remove))
(def concatv (comp vec concat))
(def takev (comp vec take))
(def restv (comp vec rest))
(def butlastv (comp vec butlast))
(def reversev (comp vec reverse))
(def sortv (comp vec sort))
(def sortv-by (comp vec sort-by))

(def rangev
  ;; Cap the range to avoid memory errors.
  (comp vec #(take 100 %) range))

(defn index-of
  [coll el]
  (.indexOf coll el))

(defn in?
  [coll el]
  (<= 0 (.indexOf coll el)))

(defn replacev
  [vtr to-replace replace-with]
  (replace {to-replace replace-with} vtr))

(defn replacev-first
  [vtr to-replace replace-with]
  (let [idx (.indexOf vtr to-replace)]
    (if (< idx 0)
      vtr
      (assoc vtr idx replace-with))))

(defn remove-element
  [vtr el]
  (vec (remove #{el} vtr)))

(defn safe-subvec
  [vtr start end]
  (let [start (min (count vtr) (max 0 start))
        end (min (count vtr) (max start end))]
    (subvec vtr start end)))

(defn safe-assoc-nth
  [vtr idx el]
  (if (empty? vtr)
    vtr
    (let [idx (mod idx (count vtr))]
      (assoc vtr idx el))))

(defn safe-nth
  [coll idx]
  (if (empty? coll)
    (throw (ex-info "Cannot take safe-nth of empty vector." {:coll coll :idx idx}))
    (let [idx (mod idx (count coll))]
      (nth coll idx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set

(defn map-set [f s] (into #{} (map f s)))
(defn filter-set [pred s] (into #{} (filter pred s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map

(defn ->map
  [coll]
  (into {} coll))

(defn filter-map [pred m] (into {} (filter pred m)))
(def keys-vec (comp vec keys))
(def keys-set (comp set keys))
(def vals-vec (comp vec vals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tuple

(defn assoc-left [tuple val] (assoc tuple 0 val))
(defn assoc-right [tuple val] (assoc tuple 1 val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground Schemas
;; nil? boolean? int? double? char? string? keyword?

(def NIL {:type 'nil?})
(def BOOLEAN {:type 'boolean?})
(def INT {:type 'int?})
(def DOUBLE {:type 'double?})
(def CHAR {:type 'char?})
(def STRING {:type 'string?})
(def KEYWORD {:type 'keyword?})

(defn unary-transform
  [type]
  {:type   :=>
   :input  {:type :cat :children [type]}
   :output type})

(defn binary-transform
  [type]
  {:type   :=>
   :input  {:type :cat :children [type type]}
   :output type})

(defn unary-pred
  [type]
  {:type   :=>
   :input  {:type :cat :children [type]}
   :output {:type 'boolean?}})

(defn binary-pred
  [type]
  {:type   :=>
   :input  {:type :cat :children [type type]}
   :output {:type 'boolean?}})

(defn fn-of
  [args ret]
  {:type   :=>
   :input  {:type :cat :children (vec args)}
   :output ret})

(defn s-var
  [sym]
  {:type :s-var :sym sym})

(defn vector-of
  [el]
  {:type :vector :child el})

(defn map-of
  [k v]
  {:type :map-of :key k :value v})

(defn set-of
  [el]
  {:type :set :child el})

(defn tuple-of
  [& els]
  {:type :tuple :children (vec els)})

(defn scheme [schema] (schema/generalize {} schema))

(def type-env
  {;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; FP
   'comp2-fn1          (scheme (fn-of [(fn-of [(s-var 'b)] (s-var 'c))
                                       (fn-of [(s-var 'a)] (s-var 'b))]
                                      (fn-of [(s-var 'a)] (s-var 'c))))
   'comp3-fn1          (scheme (fn-of [(fn-of [(s-var 'c)] (s-var 'd))
                                       (fn-of [(s-var 'b)] (s-var 'c))
                                       (fn-of [(s-var 'a)] (s-var 'b))]
                                      (fn-of [(s-var 'a)] (s-var 'd))))
   'comp2-fn2          (scheme (fn-of [(fn-of [(s-var 'c)] (s-var 'd))
                                       (fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))]
                                      (fn-of [(s-var 'a) (s-var 'b)] (s-var 'd))))
   'comp3-fn2          (scheme (fn-of [(fn-of [(s-var 'd)] (s-var 'e))
                                       (fn-of [(s-var 'c)] (s-var 'd))
                                       (fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))]
                                      (fn-of [(s-var 'a) (s-var 'b)] (s-var 'e))))
   'partial1-fn2       (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))
                                       (s-var 'a)]
                                      (fn-of [(s-var 'b)] (s-var 'c))))
   'partial1-fn3       (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'b) (s-var 'c)] (s-var 'd))
                                       (s-var 'a)]
                                      (fn-of [(s-var 'b) (s-var 'c)] (s-var 'd))))
   'partial2-fn3       (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'b) (s-var 'c)] (s-var 'd))
                                       (s-var 'a)
                                       (s-var 'b)]
                                      (fn-of [(s-var 'c)] (s-var 'd))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Conditional Control Flow
   'if                 {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [BOOLEAN (s-var 'a) (s-var 'a)]
                                       (s-var 'a))}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Common
   '=                  {:type   :scheme
                        :s-vars ['a 'b]
                        :body   (fn-of [(s-var 'a) (s-var 'b)] BOOLEAN)}
   'not=               {:type   :scheme
                        :s-vars ['a 'b]
                        :body   (fn-of [(s-var 'a) (s-var 'b)] BOOLEAN)}
   `<'                 (scheme (fn-of [(s-var 'a) (s-var 'a)] BOOLEAN))
   `<='                (scheme (fn-of [(s-var 'a) (s-var 'a)] BOOLEAN))
   `>'                 (scheme (fn-of [(s-var 'a) (s-var 'a)] BOOLEAN))
   `>='                (scheme (fn-of [(s-var 'a) (s-var 'a)] BOOLEAN))
   ;; @todo Multiple arity of min/max
   `min'               (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)))
   `max'               (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Numeric
   'int-add            (binary-transform INT)
   'int-sub            (binary-transform INT)
   'int-mult           (binary-transform INT)
   'int-div            (fn-of [INT INT] DOUBLE)
   'int-quot           (binary-transform INT)
   'int-mod            (binary-transform INT)
   'int-inc            (unary-transform INT)
   'int-dec            (unary-transform INT)
   ;'int-lt              (binary-pred INT)
   ;'int-gt              (binary-pred INT)
   ;'int-le              (binary-pred INT)
   ;'int-ge              (binary-pred INT)
   'double-add         (binary-transform DOUBLE)
   'double-sub         (binary-transform DOUBLE)
   'double-mult        (binary-transform DOUBLE)
   'double-div         (binary-transform DOUBLE)
   'double-quot        (binary-transform DOUBLE)
   'double-mod         (binary-transform DOUBLE)
   'double-inc         (unary-transform DOUBLE)
   'double-dec         (unary-transform DOUBLE)
   ;'double-lt           (binary-pred DOUBLE)
   ;'double-gt           (binary-pred DOUBLE)
   ;'double-le           (binary-pred DOUBLE)
   ;'double-ge           (binary-pred DOUBLE)
   'int                (fn-of [DOUBLE] INT)
   'double             (fn-of [INT] DOUBLE)
   'char->int          (fn-of [CHAR] INT)
   ;'min-int             (binary-transform INT)
   ;'min-double          (binary-transform DOUBLE)
   ;'max-int             (binary-transform INT)
   ;'max-double          (binary-transform DOUBLE)
   `sin                (unary-transform DOUBLE)
   `cos                (unary-transform DOUBLE)
   `tan                (unary-transform DOUBLE)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Text
   'str                {:type   :scheme
                        :s-vars ['t]
                        :body   (fn-of [(s-var 't)] STRING)}
   `int->char          (fn-of [INT] CHAR)
   `whitespace?        (unary-pred CHAR)
   `digit?             (unary-pred CHAR)
   `letter?            (unary-pred CHAR)
   `concat-str         (fn-of [STRING STRING] STRING)
   'append-str         (fn-of [STRING CHAR] STRING)
   `take-str           (fn-of [INT STRING] STRING)
   `safe-subs          (fn-of [STRING INT INT] STRING)
   `filter-str         (fn-of [(fn-of [CHAR] BOOLEAN) STRING] STRING)
   'first-str          (fn-of [STRING] CHAR)
   'last-str           (fn-of [STRING] CHAR)
   `rest-str           (unary-transform STRING)
   `butlast-str        (unary-transform STRING)
   'nth-str            (fn-of [STRING INT] CHAR)
   'length             (fn-of [STRING] INT)
   'map-str            {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(fn-of [CHAR] (s-var 'a))
                                        STRING]
                                       (vector-of (s-var 'a)))}
   `str/reverse        (unary-transform STRING)
   'string->chars      (fn-of [STRING] (vector-of CHAR))
   `split-str          (fn-of [STRING STRING] (vector-of STRING))
   'split-str-on-char  (fn-of [STRING CHAR] (vector-of STRING))
   `split-str-on-ws    (fn-of [STRING] (vector-of STRING))
   'empty-str?         (unary-pred STRING)
   `str/includes?      (binary-pred STRING)
   `char-in?           (fn-of [STRING CHAR] BOOLEAN)
   'index-of-char      (fn-of [STRING CHAR] INT)
   'index-of-str       (fn-of [STRING STRING] INT)
   'char-occurrences   (fn-of [STRING CHAR] INT)
   `str/replace        (fn-of [STRING STRING STRING] STRING)
   `str/replace-first  (fn-of [STRING STRING STRING] STRING)
   `replace-char       (fn-of [STRING CHAR CHAR] STRING)
   `replace-first-char (fn-of [STRING CHAR CHAR] STRING)
   `remove-char        (fn-of [STRING CHAR] STRING)
   `set-char           (fn-of [STRING INT CHAR] STRING)
   `str/join           (fn-of [(vector-of STRING)] STRING)
   'str-join-sep       (fn-of [STRING (vector-of STRING)] STRING)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Boolean
   `and                (binary-transform BOOLEAN)
   `or                 (binary-transform BOOLEAN)
   'not                (unary-transform BOOLEAN)
   'zero-int?          (fn-of [INT] BOOLEAN)
   'zero-double?       (fn-of [DOUBLE] BOOLEAN)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Vector
   '->vector1          (scheme (fn-of [(s-var 'a)]
                                      (vector-of (s-var 'a))))
   '->vector2          (scheme (fn-of [(s-var 'a) (s-var 'a)]
                                      (vector-of (s-var 'a))))
   '->vector3          (scheme (fn-of [(s-var 'a) (s-var 'a) (s-var 'a)]
                                      (vector-of (s-var 'a))))
   'map->vec           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
                                      (vector-of (tuple-of (s-var 'k) (s-var 'v)))))
   'set->vec           (scheme (fn-of [(set-of (s-var 'e))] (vector-of (s-var 'e))))
   `concatv            {:type   :scheme
                        :s-vars ['a]
                        :body   (binary-transform (vector-of (s-var 'a)))}
   'vec-conj           {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a)) (s-var 'a)] (vector-of (s-var 'a)))}
   `takev              {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [INT (vector-of (s-var 'a))]
                                       (vector-of (s-var 'a)))}
   `safe-subvec        {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a)) INT INT]
                                       (vector-of (s-var 'a)))}
   'first              {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a))] (s-var 'a))}
   'last               {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a))] (s-var 'a))}
   `restv              {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a))] (vector-of (s-var 'a)))}
   `butlastv           {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a))] (vector-of (s-var 'a)))}
   `safe-nth           {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a)) INT] (s-var 'a))}
   `reversev           {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a))] (vector-of (s-var 'a)))}
   'empty?             {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a))] BOOLEAN)}
   `in?                {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a)) (s-var 'a)] BOOLEAN)}
   `index-of           {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a)) (s-var 'a)] INT)}
   `occurrences-of     {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a)) (s-var 'a)] INT)}
   `safe-assoc-nth     {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a))
                                        INT
                                        (s-var 'a)]
                                       (vector-of (s-var 'a)))}
   `replacev           {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a))
                                        (s-var 'a)
                                        (s-var 'a)]
                                       (vector-of (s-var 'a)))}
   `replacev-first     {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a))
                                        (s-var 'a)
                                        (s-var 'a)]
                                       (vector-of (s-var 'a)))}
   `remove-element     {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(vector-of (s-var 'a)) (s-var 'a)]
                                       (vector-of (s-var 'a)))}
   'range1             (scheme (fn-of [INT] (vector-of INT)))
   'range2             (scheme (fn-of [INT INT] (vector-of INT)))
   'range3             (scheme (fn-of [INT INT INT] (vector-of INT)))
   'count-vec          (scheme (fn-of [(vector-of (s-var 'a))] INT))
   'map-vec            {:type   :scheme
                        :s-vars ['a 'b]
                        :body   (fn-of [(fn-of [(s-var 'a)] (s-var 'b))
                                        (vector-of (s-var 'a))]
                                       (vector-of (s-var 'b)))}
   'map2-vec           {:type   :scheme
                        :s-vars ['a1 'a2 'b]
                        :body   (fn-of [(fn-of [(s-var 'a1) (s-var 'a2)] (s-var 'b))
                                        (vector-of (s-var 'a1))
                                        (vector-of (s-var 'a2))]
                                       (vector-of (s-var 'b)))}
   `mapv-indexed       (scheme (fn-of [(fn-of [INT (s-var 'a)] (s-var 'b))
                                       (vector-of (s-var 'a))]
                                      (vector-of (s-var 'b))))
   'filterv            {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
                                        (vector-of (s-var 'a))]
                                       (vector-of (s-var 'a)))}
   'reduce-vec         {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(fn-of [(s-var 'a) (s-var 'a)] (s-var 'a))
                                        (vector-of (s-var 'a))]
                                       (s-var 'a))}
   'fold-vec           {:type   :scheme
                        :s-vars ['a 'b]
                        :body   (fn-of [(fn-of [(s-var 'b) (s-var 'a)] (s-var 'b))
                                        (s-var 'b)
                                        (vector-of (s-var 'a))]
                                       (s-var 'b))}

   `removev            {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
                                        (vector-of (s-var 'a))]
                                       (vector-of (s-var 'a)))}
   `mapcatv            {:type   :scheme
                        :s-vars ['a 'b]
                        :body   (fn-of [(fn-of [(s-var 'a)] (vector-of (s-var 'b)))
                                        (vector-of (s-var 'a))]
                                       (vector-of (s-var 'b)))}

   `distinctv          (scheme (fn-of [(vector-of (s-var 'e))]
                                      (vector-of (s-var 'e))))
   `sortv              (scheme (fn-of [(vector-of (s-var 'e))]
                                      (vector-of (s-var 'e))))
   `sortv-by           (scheme (fn-of [(fn-of [(s-var 'e)] (s-var 'k))
                                       (vector-of (s-var 'e))]
                                      (vector-of (s-var 'e))))
   'group-by           (scheme (fn-of [(fn-of [(s-var 'e)] (s-var 'k))
                                       (vector-of (s-var 'e))]
                                      (map-of (s-var 'k) (vector-of (s-var 'e)))))
   'zipmap             (scheme (fn-of [(vector-of (s-var 'k))
                                       (vector-of (s-var 'v))]
                                      (map-of (s-var 'k) (s-var 'v))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;; Tuple
   '->tuple2           (scheme (fn-of [(s-var 'a) (s-var 'b)]
                                      (tuple-of (s-var 'a) (s-var 'b))))
   'left               (scheme (fn-of [(tuple-of (s-var 'a) (s-var 'b))]
                                      (s-var 'a)))
   'right              (scheme (fn-of [(tuple-of (s-var 'a) (s-var 'b))]
                                      (s-var 'b)))
   `assoc-left         (scheme (fn-of [(tuple-of (s-var 'a) (s-var 'b)) (s-var 'c)]
                                      (tuple-of (s-var 'c) (s-var 'b))))
   `assoc-right        (scheme (fn-of [(tuple-of (s-var 'a) (s-var 'b)) (s-var 'c)]
                                      (tuple-of (s-var 'a) (s-var 'c))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Set
   '->set1             (scheme (fn-of [(s-var 'e)]
                                      (set-of (s-var 'e))))
   '->set2             (scheme (fn-of [(s-var 'e) (s-var 'e)]
                                      (set-of (s-var 'e))))
   '->set3             (scheme (fn-of [(s-var 'e) (s-var 'e) (s-var 'e)]
                                      (set-of (s-var 'e))))
   'vec->set           (scheme (fn-of [(vector-of (s-var 'e))]
                                      (set-of (s-var 'e))))
   'map->set           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
                                      (set-of (tuple-of (s-var 'k) (s-var 'v)))))
   `set/union          (scheme (fn-of [(set-of (s-var 'e))
                                       (set-of (s-var 'e))]
                                      (set-of (s-var 'e))))
   `set/difference     (scheme (fn-of [(set-of (s-var 'e))
                                       (set-of (s-var 'e))]
                                      (set-of (s-var 'e))))
   `set/intersection   (scheme (fn-of [(set-of (s-var 'e))
                                       (set-of (s-var 'e))]
                                      (set-of (s-var 'e))))
   `set/subset?        (scheme (fn-of [(set-of (s-var 'e))
                                       (set-of (s-var 'e))]
                                      BOOLEAN))
   `set/superset?      (scheme (fn-of [(set-of (s-var 'e))
                                       (set-of (s-var 'e))]
                                      BOOLEAN))
   'set-conj           (scheme (fn-of [(set-of (s-var 'e)) (s-var 'e)]
                                      (set-of (s-var 'e))))
   'disj               (scheme (fn-of [(set-of (s-var 'e)) (s-var 'e)]
                                      (set-of (s-var 'e))))
   'set-contains?      (scheme (fn-of [(set-of (s-var 'e)) (s-var 'e)]
                                      BOOLEAN))
   'count-set          (scheme (fn-of [(set-of (s-var 'e))] INT))
   `map-set            (scheme (fn-of [(fn-of [(s-var 'a)] (s-var 'b))
                                       (set-of (s-var 'a))]
                                      (set-of (s-var 'b))))
   `filter-set         (scheme (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
                                       (set-of (s-var 'a))]
                                      (set-of (s-var 'a))))
   'reduce-set         (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'a)] (s-var 'a))
                                       (set-of (s-var 'a))]
                                      (s-var 'a)))
   'fold-set           (scheme (fn-of [(fn-of [(s-var 'b) (s-var 'a)] (s-var 'b))
                                       (s-var 'b)
                                       (set-of (s-var 'a))]
                                      (s-var 'b)))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;; Map
   '->map1             (scheme (fn-of [(s-var 'k) (s-var 'v)]
                                      (map-of (s-var 'k) (s-var 'v))))
   '->map2             (scheme (fn-of [(s-var 'k) (s-var 'v)
                                       (s-var 'k) (s-var 'v)]
                                      (map-of (s-var 'k) (s-var 'v))))
   '->map3             (scheme (fn-of [(s-var 'k) (s-var 'v)
                                       (s-var 'k) (s-var 'v)
                                       (s-var 'k) (s-var 'v)]
                                      (map-of (s-var 'k) (s-var 'v))))
   'vec->map           (scheme (fn-of [(vector-of (tuple-of (s-var 'k) (s-var 'v)))]
                                      (map-of (s-var 'k) (s-var 'v))))
   'set->map           (scheme (fn-of [(set-of (tuple-of (s-var 'k) (s-var 'v)))]
                                      (map-of (s-var 'k) (s-var 'v))))
   'get                (scheme (fn-of [(map-of (s-var 'k) (s-var 'v)) (s-var 'k)]
                                      (s-var 'v)))
   'get-or-else        (scheme (fn-of [(map-of (s-var 'k) (s-var 'v)) (s-var 'k) (s-var 'v)]
                                      (s-var 'v)))
   'assoc              (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))
                                       (s-var 'k)
                                       (s-var 'v)]
                                      (map-of (s-var 'k) (s-var 'v))))
   'update             (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))
                                       (s-var 'k)
                                       (fn-of [(s-var 'v)] (s-var 'v))]
                                      (map-of (s-var 'k) (s-var 'v))))
   'map-contains?      (scheme (fn-of [(map-of (s-var 'k) (s-var 'v)) (s-var 'k)]
                                      BOOLEAN))
   `keys-vec           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
                                      (vector-of (s-var 'k))))
   `keys-set           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
                                      (set-of (s-var 'k))))
   `vals-vec           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
                                      (vector-of (s-var 'v))))
   'merge              (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))
                                       (map-of (s-var 'k) (s-var 'v))]
                                      (map-of (s-var 'k) (s-var 'v))))
   'count-map          (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
                                      INT))
   'map-map            (scheme (fn-of [(fn-of [(tuple-of (s-var 'k) (s-var 'v))] (s-var 'e))
                                       (map-of (s-var 'k) (s-var 'v))]
                                      (vector-of (s-var 'e))))
   `filter-map         (scheme (fn-of [(fn-of [(tuple-of (s-var 'k) (s-var 'v))] BOOLEAN)
                                       (map-of (s-var 'k) (s-var 'v))]
                                      (map-of (s-var 'k) (s-var 'v))))
   'reduce-map         (let [entry (tuple-of (s-var 'k) (s-var 'v))]
                         (scheme (fn-of [(fn-of [entry entry] entry)
                                         (map-of (s-var 'k) (s-var 'v))]
                                        entry)))
   'fold-map           (scheme (fn-of [(fn-of [(s-var 'r)
                                               (tuple-of (s-var 'k) (s-var 'v))]
                                              (s-var 'r))
                                       (s-var 'r)
                                       (map-of (s-var 'k) (s-var 'v))]
                                      (s-var 'r)))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Printing & Side Effects
   'do2                {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [NIL (s-var 'a)] (s-var 'a))}
   'do3                {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [NIL NIL (s-var 'a)] (s-var 'a))}
   'print              {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(s-var 'a)] NIL)}
   'println            {:type   :scheme
                        :s-vars ['a]
                        :body   (fn-of [(s-var 'a)] NIL)}
   })

(def dealiases
  '{->map1            hash-map
    ->map2            hash-map
    ->map3            hash-map
    ->set1            hash-set
    ->set2            hash-set
    ->set3            hash-set
    ->tuple2          vector
    ->vector1         vector
    ->vector2         vector
    ->vector3         vector
    append-str        str
    char->int         int
    char-occurrences  erp12.cbgp-lite.lang.lib/occurrences-of
    comp2-fn1         comp
    comp2-fn2         comp
    comp3-fn1         comp
    comp3-fn2         comp
    count-map         count
    count-set         count
    count-vec         count
    do2               do
    do3               do
    double-add        +
    double-dec        dec
    double-div        erp12.cbgp-lite.lang.lib/safe-div
    double-inc        inc
    double-mod        erp12.cbgp-lite.lang.lib/safe-mod
    double-mult       *
    double-quot       erp12.cbgp-lite.lang.lib/safe-quot
    double-sub        -
    empty-str?        empty?
    first-str         first
    fold-vec          reduce
    fold-map          reduce
    fold-set          reduce
    get-or-else       get
    index-of-char     clojure.string/index-of
    index-of-str      clojure.string/index-of
    int-add           +
    int-dec           dec
    int-div           erp12.cbgp-lite.lang.lib/safe-div
    int-inc           inc
    int-mod           erp12.cbgp-lite.lang.lib/safe-mod
    int-mult          *
    int-quot          erp12.cbgp-lite.lang.lib/safe-quot
    int-sub           -
    last-str          last
    left              first
    length            count
    map->set          set
    map->vec          vec
    map-contains?     contains?
    map-map           mapv
    map-str           mapv
    map-vec           mapv
    map2-vec          mapv
    nth-or-else       nth
    nth-str           erp12.cbgp-lite.lang.lib/safe-nth
    partial1-fn2      partial
    partial1-fn3      partial
    partial2-fn3      partial
    range1            erp12.cbgp-lite.lang.lib/rangev
    range2            erp12.cbgp-lite.lang.lib/rangev
    range3            erp12.cbgp-lite.lang.lib/rangev
    reduce-vec        reduce
    reduce-map        reduce
    reduce-set        reduce
    right             second
    set->map          erp12.cbgp-lite.lang.lib/->map
    set->vec          vec
    set-conj          conj
    set-contains?     contains?
    split-str-on-char erp12.cbgp-lite.lang.lib/split-str
    str-join-sep      clojure.string/join
    string->chars     vec
    vec->map          erp12.cbgp-lite.lang.lib/->map
    vec->set          set
    vec-conj          conj
    vec-mapv          mapv
    zero-double?      zero?
    zero-int?         zero?
    })

(def macros
  #{'if 'do2 'do3})

(defn lib-for-types
  [types]
  (->> type-env
       (filter (fn [[_ typ]]
                 (core/or (= (:type typ) :scheme)
                          (some #(schema/occurs? % typ) types))))
       (into {})))
