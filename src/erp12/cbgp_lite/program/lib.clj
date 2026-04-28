(ns erp12.cbgp-lite.program.lib
  "The library of functions and their types supported by cbgp."
  (:require [clj-memory-meter.core :as mm]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as str]
            [erp12.cbgp-lite.program.types :refer [fn-scheme fn-type vec-type set-type tuple-type map-type
                                                   NIL BOOL INT FLOAT CHAR STRING]]))

(def VALUE-MAX-BYTES 50000) ;; 50 KiB

(defn guard
  [x]
  (let [num-bytes (mm/measure x :bytes true)]
    (if (> num-bytes VALUE-MAX-BYTES)
      (throw (ex-info "Value too large."
                      ;; Don't put the full value in error data because it will OOM later.
                      {:class (type x)
                       :bytes num-bytes}))
      x)))

(defn guarded-reduce
  ([f coll]
   (reduce (comp guard f) coll))
  ([f init coll]
   (reduce (comp guard f) init coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Safe Functions

(defn safe-div
  [n d]
  (if (zero? d) 0 (/ n d)))

(defn safe-quot
  [n d]
  (if (zero? d) 0 (quot n d)))

(defn safe-mod
  [n d]
  (if (zero? d) 0 (mod n d)))

(defn safe-pow
  [x y]
  (let [result (math/pow x y)]
    (if (or (NaN? result) (infinite? result))
      (throw (ex-info "Pow resulting in undefined value." {:base     x
                                                           :exponent y}))
      result)))

(defn safe-square
  [x]
  (safe-pow x 2))

(defn- safe-trig-x
  [x]
  (dec (mod (inc x) 2)))

(defn safe-acos
  [x]
  (if (= 1.0 (mod x 2))
    0.0
    (let [safe-x (safe-trig-x x)]
      (Math/acos safe-x))))

(defn safe-asin
  [x]
  (if (= 1.0 (mod x 2))
    (/ Math/PI 2)
    (let [safe-x (safe-trig-x x)]
      (Math/asin safe-x))))

(defn safe-log2
  [x]
  (let [safe-x (if (<= x 0) Float/MIN_VALUE x)]
    (/ (Math/log safe-x)
       (Math/log 2))))

(defn safe-log10
  [x]
  (let [safe-x (if (<= x 0) Float/MIN_VALUE x)]
    (Math/log10 safe-x)))

(defn int->char
  [i]
  (char (mod i 128)))

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

(defn index-of
  [coll el]
  (.indexOf coll el))

(defn occurrences
  [coll el]
  (count (filter #{el} coll)))

(defn vec-contains?
  [coll el]
  (<= 0 (.indexOf coll el)))

(defn set-char
  [s idx c]
  (if (empty? s)
    s
    (let [safe-idx (mod idx (count s))]
      (apply str (assoc (vec s) safe-idx c)))))

(defn safe-slice
  [coll slice-fn start end]
  (let [start (min (count coll) (max 0 start))
        end   (min (count coll) (max start end))]
    (slice-fn coll start end)))

(defn safe-subs
  [s start end]
  (safe-slice s subs start end))

(defn safe-subvec
  [vtr start end]
  (safe-slice vtr subvec start end))

(defn safe-nth
  [coll idx]
  (if (empty? coll)
    (throw (ex-info "Cannot take safe-nth of empty vector." {:coll coll
                                                             :idx  idx}))
    (let [idx (mod idx (count coll))]
      (nth coll idx))))

(defn safe-assoc-nth
  [vtr idx el]
  (if (empty? vtr)
    vtr
    (let [idx (mod idx (count vtr))]
      (assoc vtr idx el))))

(defn vec-replace
  [vtr to-replace replace-with]
  (replace {to-replace replace-with} vtr))

(defn vec-replace-first
  [vtr to-replace replace-with]
  (let [idx (.indexOf vtr to-replace)]
    (if (< idx 0)
      vtr
      (assoc vtr idx replace-with))))

(defn assoc-tuple
  [tup i x]
  ;; @todo Why do we provide a default tuple?
  (assoc (or tup [nil nil]) i x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases

(def comp2-fn1 comp)
(def comp3-fn1 comp)
(def comp2-fn2 comp)
(def comp3-fn2 comp)
(def partial1-fn2 partial)
(def partial1-fn3 partial)
(def partial2-fn3 partial)
(defn <' [a b] (< (compare a b) 0))
(defn <=' [a b] (or (= a b) (<' a b)))
(defn >' [a b] (not (<=' a b)))
(defn >=' [a b] (not (<' a b)))
(defn min' [a b] (if (<' a b) a b))
(defn max' [a b] (if (<' a b) b a))
(def int-add +)
(def int-sub -)
(def int-mult *)
(def int-div safe-div)
(def int-quot safe-quot)
(def int-mod safe-mod)
(def int-inc inc)
(def int-dec dec)
(def int-neg -)
(def int-abs abs)
(def int-pow (comp long safe-pow))
(def int-square (comp long safe-square))
(def int-ceil (comp long math/ceil))
(def int-floor (comp long math/ceil))
(def float-add +)
(def float-sub -)
(def float-mult *)
(def float-div safe-div)
(def float-quot safe-quot)
(def float-mod safe-mod)
(def float-inc inc)
(def float-dec dec)
(def float-neg -)
(def float-abs abs)
(def float-pow safe-pow)
(def float-square safe-square)
(def char->int int)
(def safe-sqrt (comp math/sqrt abs))
(defn whitespace? [^Character c] (Character/isWhitespace c))
(defn digit? [^Character c] (Character/isDigit c))
(defn letter? [^Character c] (Character/isLetter c))
(defn char-upper [^Character c] (Character/toUpperCase c))
(defn char-lower [^Character c] (Character/toLowerCase c))
(def concat-s (comp guard str))
(def append-s str)
(def take-s (comp str/join take))
(def filter-str (comp str/join filter))
(def str-first first)
(def str-last last)
(def str-rest (comp str/join rest))
(def str-butlast (comp str/join butlast))
(def str-nth nth)
(def str-count count)
(def str-replace (comp guard str/replace))
(def str-replace-first (comp guard str/replace-first))
(def map-str (comp guard mapv))
(def mapcat-vec (comp guard vec mapcat))
(def mapcat-str mapcat-vec)
(def str-chars vec)
(def split-str-on-char split-str)
(def str-empty? empty?)
(def index-of-char str/index-of)
(def index-of-str str/index-of)
(def char-occurrences occurrences)
(defn replace-char [s c1 c2] (str/replace s (str c1) (str c2)))
(defn replace-first-char [s c1 c2] (str/replace-first s (str c1) (str c2)))
(defn remove-char [s c] (str/join (remove #{c} s)))
(def str-join-on str/join)
(def join-chars str/join)
(def sort-str (comp str/join sort))
(defn and' [a b] (and a b))
(defn or' [a b] (or a b))
(def zero-int? zero?)
(def zero-float? zero?)
(def ->vector1 vector)
(def ->vector2 vector)
(def ->vector3 vector)
(def map->vec vec)
(def set->vec vec)
(def concat-v (comp guard vec concat))
(def conj-v (comp guard conj))
(def take-v (comp vec take))
(def vec-rest (comp vec rest))
(def vec-butlast (comp vec butlast))
(def nth-or-else nth)
(def vec-reverse (comp vec reverse))
(def vec-empty? empty?)
(def ->tuple2 vector)
(def left first)
(def right second)
(defn assoc-left [tuple val] (assoc-tuple tuple 0 val))
(defn assoc-right [tuple val] (assoc-tuple tuple 1 val))
(def ->set1 hash-set)
(def ->set2 hash-set)
(def ->set3 hash-set)
(def vec->set set)
(def map->set set)
(def conj-set (comp guard conj))
(def set-contains? contains?)
(def map-set (comp guard set map))
(def filter-set (comp set filter))
(def reduce-set guarded-reduce)
(def fold-set guarded-reduce)
(def set-union (comp guard set/union))
(def ->map1 hash-map)
(def ->map2 hash-map)
(def ->map3 hash-map)
(defn ->map [coll] (into {} coll))
(def vec->map ->map)
(def set->map ->map)
(def get-or-else get)
(def safe-assoc (comp guard assoc))
(def conj-map (comp guard conj))
(def map-contains? contains?)
(def keys-vec (comp vec keys))
(def keys-set (comp set keys))
(def vals-vec (comp vec vals))
(def count-map count)
(def mapv-map (comp guard mapv))
(def filter-map (comp ->map filter))
(def reduce-map guarded-reduce)
(def fold-map guarded-reduce)
(defn vec-remove-el [s c] (vec (remove #{c} s)))
(def range1 (comp vec #(take 100 %) range))
(def range2 range1)
(def range3 range1)
(def count-vec count)
(def mapv2 (comp guard vec map))
(def mapv-indexed (comp guard vec map-indexed))
(def reduce-vec guarded-reduce)
(def fold-vec guarded-reduce)
(def remove-vec (comp vec remove))
(def distinct-vec (comp vec distinct))
(def sort-vec (comp vec sort))
(def sort-by-vec (comp vec sort-by))
(def safe-merge (comp guard merge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prelude

(def type-env
  {`comp2-fn1          (fn-scheme {:kinds [:* :* :*]
                                   :args  [(fn-type [1] 2)
                                           (fn-type [0] 1)]
                                   :ret   (fn-type [0] 2)})
   `comp3-fn1          (fn-scheme {:kinds [:* :* :* :*]
                                   :args  [(fn-type [2] 3)
                                           (fn-type [1] 2)
                                           (fn-type [0] 1)]
                                   :ret   (fn-type [0] 3)})
   `comp2-fn2          (fn-scheme {:kinds [:* :* :* :*]
                                   :args  [(fn-type [2] 3)
                                           (fn-type [0 1] 2)]
                                   :ret   (fn-type [0 1] 3)})
   `comp3-fn2          (fn-scheme {:kinds [:* :* :* :* :*]
                                   :args  [(fn-type [3] 4)
                                           (fn-type [2] 3)
                                           (fn-type [0 1] 2)]
                                   :ret   (fn-type [0 1] 4)})
   `partial1-fn2       (fn-scheme {:kinds [:* :* :*]
                                   :args  [(fn-type [0 1] 2)
                                           0]
                                   :ret   (fn-type [1] 2)})
   `partial1-fn3       (fn-scheme {:kinds [:* :* :* :*]
                                   :args  [(fn-type [0 1 2] 3)
                                           0]
                                   :ret   (fn-type [1 2] 3)})
   `partial2-fn3       (fn-scheme {:kinds [:* :* :* :*]
                                   :args  [(fn-type [0 1 2] 3)
                                           0
                                           1]
                                   :ret   (fn-type [2] 3)})

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Conditional Control Flow
   ;; Use unqualified symbol for `if` because it is a special symbol.    
   'if                 (fn-scheme {:kinds [:*]
                                   :args  [BOOL 0 0]
                                   :ret   0})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Common
   `=                  (fn-scheme {:kinds [:*]
                                   :args  [0 0]
                                   :ret   BOOL})
   `not=               (fn-scheme {:kinds [:*]
                                   :args  [0 0]
                                   :ret   BOOL})

   `<'                 (fn-scheme {:kinds [:*]
                                   :args  [0 0]
                                   :ret   BOOL})
   `<='                (fn-scheme {:kinds [:*]
                                   :args  [0 0]
                                   :ret   BOOL})
   `>'                 (fn-scheme {:kinds [:*]
                                   :args  [0 0]
                                   :ret   BOOL})
   `>='                (fn-scheme {:kinds [:*]
                                   :args  [0 0]
                                   :ret   BOOL})
   `min'               (fn-scheme {:kinds [:*]
                                   :args  [0 0]
                                   :ret   0})
   `max'               (fn-scheme {:kinds [:*]
                                   :args  [0 0]
                                   :ret   0})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Numeric
   `int-add            (fn-scheme {:args [INT INT]
                                   :ret  INT})
   `int-sub            (fn-scheme {:args [INT INT]
                                   :ret  INT})
   `int-mult           (fn-scheme {:args [INT INT]
                                   :ret  INT})
   `int-div            (fn-scheme {:args [INT INT]
                                   :ret  FLOAT})
   `int-quot           (fn-scheme {:args [INT INT]
                                   :ret  INT})
   `int-mod            (fn-scheme {:args [INT INT]
                                   :ret  INT})
   `int-inc            (fn-scheme {:args [INT]
                                   :ret  INT})
   `int-dec            (fn-scheme {:args [INT]
                                   :ret  INT})
   `int-neg            (fn-scheme {:args [INT]
                                   :ret  INT})
   `int-abs            (fn-scheme {:args [INT]
                                   :ret  INT})
   `int-pow            (fn-scheme {:args [INT INT]
                                   :ret  INT})
   `int-square         (fn-scheme {:args [INT]
                                   :ret  INT})
   `int-ceil           (fn-scheme {:args [FLOAT]
                                   :ret  INT})
   `int-floor          (fn-scheme {:args [FLOAT]
                                   :ret  INT})
   `float-add          (fn-scheme {:args [FLOAT FLOAT]
                                   :ret  FLOAT})
   `float-sub          (fn-scheme {:args [FLOAT FLOAT]
                                   :ret  FLOAT})
   `float-mult         (fn-scheme {:args [FLOAT FLOAT]
                                   :ret  FLOAT})
   `float-div          (fn-scheme {:args [FLOAT FLOAT]
                                   :ret  FLOAT})
   `float-quot         (fn-scheme {:args [FLOAT FLOAT]
                                   :ret  FLOAT})
   `float-mod          (fn-scheme {:args [FLOAT FLOAT]
                                   :ret  FLOAT})
   `float-inc          (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `float-dec          (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `float-neg          (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `float-abs          (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `float-pow          (fn-scheme {:args [FLOAT FLOAT]
                                   :ret  FLOAT})
   `float-square       (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `int                (fn-scheme {:args [FLOAT]
                                   :ret  INT})
   `float              (fn-scheme {:args [INT]
                                   :ret  FLOAT})
   `char->int          (fn-scheme {:args [CHAR]
                                   :ret  INT})
   `safe-sqrt          (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `math/sin           (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `math/cos           (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `math/tan           (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `safe-asin          (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `safe-acos          (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `math/atan          (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `safe-log2          (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `safe-log10         (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `math/ceil          (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   `math/floor         (fn-scheme {:args [FLOAT]
                                   :ret  FLOAT})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Text
   `str                (fn-scheme {:kinds [:*]
                                   :args  [0]
                                   :ret   STRING})
   `int->char          (fn-scheme {:args [INT]
                                   :ret  CHAR})
   `whitespace?        (fn-scheme {:args [CHAR]
                                   :ret  BOOL})
   `digit?             (fn-scheme {:args [CHAR]
                                   :ret  BOOL})
   `letter?            (fn-scheme {:args [CHAR]
                                   :ret  BOOL})
   `concat-s           (fn-scheme {:args [STRING STRING]
                                   :ret  STRING})
   `append-s           (fn-scheme {:args [STRING CHAR]
                                   :ret  STRING})
   `take-s             (fn-scheme {:args [INT STRING]
                                   :ret  STRING})
   `safe-subs          (fn-scheme {:args [STRING INT INT]
                                   :ret  STRING})
   `filter-str         (fn-scheme {:args [(fn-type [CHAR] BOOL)
                                          STRING]
                                   :ret  STRING})
   `str-first          (fn-scheme {:args [STRING]
                                   :ret  CHAR})
   `str-last           (fn-scheme {:args [STRING]
                                   :ret  CHAR})
   `str-rest           (fn-scheme {:args [STRING]
                                   :ret  STRING})
   `str-butlast        (fn-scheme {:args [STRING]
                                   :ret  STRING})
   `str-nth            (fn-scheme {:args [STRING INT]
                                   :ret  CHAR})
   `str-count          (fn-scheme {:args [STRING]
                                   :ret  INT})
   `map-str            (fn-scheme {:kinds [:*]
                                   :args  [(fn-type [CHAR] 0)
                                           STRING]
                                   :ret   (vec-type 0)})
   `mapcat-str         (fn-scheme {:kinds [:*]
                                   :args  [(fn-type [CHAR] (vec-type 0))
                                           STRING]
                                   :ret   (vec-type 0)})
   `str/reverse        (fn-scheme {:args [STRING]
                                   :ret  STRING})
   `str-chars          (fn-scheme {:args [STRING]
                                   :ret  (vec-type CHAR)})
   `split-str          (fn-scheme {:args [STRING STRING]
                                   :ret  (vec-type STRING)})
   `split-str-on-char  (fn-scheme {:args [STRING CHAR]
                                   :ret  (vec-type STRING)})
   `split-str-on-ws    (fn-scheme {:args [STRING]
                                   :ret  (vec-type STRING)})
   `str-empty?         (fn-scheme {:args [STRING]
                                   :ret  BOOL})
   `str/includes?      (fn-scheme {:args [STRING STRING]
                                   :ret  BOOL})
   `index-of-char      (fn-scheme {:args [STRING CHAR]
                                   :ret  INT})
   `index-of-str       (fn-scheme {:args [STRING STRING]
                                   :ret  INT})
   `char-occurrences   (fn-scheme {:args [STRING CHAR]
                                   :ret  INT})
   `str-replace        (fn-scheme {:args [STRING STRING STRING]
                                   :ret  STRING})
   `str-replace-first  (fn-scheme {:args [STRING STRING STRING]
                                   :ret  STRING})
   `replace-char       (fn-scheme {:args [STRING CHAR CHAR]
                                   :ret  STRING})
   `replace-first-char (fn-scheme {:args [STRING CHAR CHAR]
                                   :ret  STRING})
   `remove-char        (fn-scheme {:args [STRING CHAR]
                                   :ret  STRING})
   `set-char           (fn-scheme {:args [STRING INT CHAR]
                                   :ret  STRING})
   `str/join           (fn-scheme {:args [(vec-type STRING)]
                                   :ret  STRING})
   `str-join-on        (fn-scheme {:args [(vec-type STRING) STRING]
                                   :ret  STRING})
   `join-chars         (fn-scheme {:args [(vec-type CHAR)]
                                   :ret  STRING})
   `str/capitalize     (fn-scheme {:args [STRING]
                                   :ret  STRING})
   `str/upper-case     (fn-scheme {:args [STRING]
                                   :ret  STRING})
   `str/lower-case     (fn-scheme {:args [STRING]
                                   :ret  STRING})
   `sort-str           (fn-scheme {:args [STRING]
                                   :ret  STRING})
   `char-upper         (fn-scheme {:args [CHAR]
                                   :ret  CHAR})
   `char-lower         (fn-scheme {:args [CHAR]
                                   :ret  CHAR})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Boolean
   `and'               (fn-scheme {:args [BOOL BOOL]
                                   :ret  BOOL})
   `or'                (fn-scheme {:args [BOOL BOOL]
                                   :ret  BOOL})
   `not                (fn-scheme {:args [BOOL]
                                   :ret  BOOL})
   `zero-int?          (fn-scheme {:args [INT]
                                   :ret  BOOL})
   `zero-float?        (fn-scheme {:args [FLOAT]
                                   :ret  BOOL})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Vector
   `->vector1          (fn-scheme {:kinds [:*]
                                   :args  [0]
                                   :ret   (vec-type 0)})
   `->vector2          (fn-scheme {:kinds [:*]
                                   :args  [0 0]
                                   :ret   (vec-type 0)})
   `->vector3          (fn-scheme {:kinds [:*]
                                   :args  [0 0 0]
                                   :ret   (vec-type 0)})
   `map->vec           (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1)]
                                   :ret   (vec-type (tuple-type [0 1]))})
   `set->vec           (fn-scheme {:kinds [:*]
                                   :args  [(set-type 0)]
                                   :ret   (vec-type 0)})
   `concat-v           (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)
                                           (vec-type 0)]
                                   :ret   (vec-type 0)})
   `conj-v             (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)
                                           0]
                                   :ret   (vec-type 0)})
   `take-v             (fn-scheme {:kinds [:*]
                                   :args  [INT
                                           (vec-type 0)]
                                   :ret   (vec-type 0)})
   `safe-subvec        (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0) INT INT]
                                   :ret   (vec-type 0)})
   `first              (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)]
                                   :ret   0})
   `last               (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)]
                                   :ret   0})
   `vec-rest           (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)]
                                   :ret   (vec-type 0)})
   `vec-butlast        (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)]
                                   :ret   (vec-type 0)})
   `safe-nth           (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0) INT]
                                   :ret   0})
   `nth-or-else        (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0) INT 0]
                                   :ret   0})
   `vec-reverse        (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)]
                                   :ret   (vec-type 0)})
   `vec-empty?         (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)]
                                   :ret   BOOL})
   `vec-contains?      (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0) 0]
                                   :ret   BOOL})
   `index-of           (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0) 0]
                                   :ret   INT})
   `occurrences        (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0) 0]
                                   :ret   BOOL})
   `safe-assoc-nth     (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)
                                           INT
                                           0]
                                   :ret   (vec-type 0)})
   `vec-replace        (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)
                                           0
                                           0]
                                   :ret   (vec-type 0)})
   `vec-replace-first  (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)
                                           0
                                           0]
                                   :ret   (vec-type 0)})
   `vec-remove-el      (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)
                                           0]
                                   :ret   (vec-type 0)})
   `range1             (fn-scheme {:args [INT]
                                   :ret  (vec-type INT)})
   `range2             (fn-scheme {:args [INT INT]
                                   :ret  (vec-type INT)})
   `range3             (fn-scheme {:args [INT INT]
                                   :ret  (vec-type INT)})
   `count-vec          (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)]
                                   :ret   INT})
   `mapv               (fn-scheme {:kinds [:* :*]
                                   :args  [(fn-type [0] 1)
                                           (vec-type 0)]
                                   :ret   (vec-type 1)})
   `mapv2              (fn-scheme {:kinds [:* :* :*]
                                   :args  [(fn-type [0 1] 2)
                                           (vec-type 0)
                                           (vec-type 1)]
                                   :ret   (vec-type 2)})
   `mapv-indexed       (fn-scheme {:kinds [:* :*]
                                   :args  [(fn-type [INT 0] 1)
                                           (vec-type 0)]
                                   :ret   (vec-type 1)})
   `filterv            (fn-scheme {:kinds [:*]
                                   :args  [(fn-type [0] BOOL)
                                           (vec-type 0)]
                                   :ret   (vec-type 0)})
   `reduce-vec         (fn-scheme {:kinds [:*]
                                   :args  [(fn-type [0 0] 0)
                                           (vec-type 0)]
                                   :ret   0})
   `fold-vec           (fn-scheme {:kinds [:* :*]
                                   :args  [(fn-type [1 0] 1)
                                           1
                                           (vec-type 0)]
                                   :ret   1})
   `remove-vec         (fn-scheme {:kinds [:*]
                                   :args  [(fn-type [0] BOOL)
                                           (vec-type 0)]
                                   :ret   (vec-type 0)})
   `mapcat-vec         (fn-scheme {:kinds [:* :*]
                                   :args  [(fn-type [0] (vec-type 1))
                                           (vec-type 0)]
                                   :ret   (vec-type 1)})
   `distinct-vec       (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)]
                                   :ret   (vec-type 0)})
   `sort-vec           (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)]
                                   :ret   (vec-type 0)})
   `sort-by-vec        (fn-scheme {:kinds [:* :*]
                                   :args  [(fn-type [0] 1)
                                           (vec-type 0)]
                                   :ret   (vec-type 0)})
   `group-by           (fn-scheme {:kinds [:* :*]
                                   :args  [(fn-type [0] 1)
                                           (vec-type 0)]
                                   :ret   (map-type 1 (vec-type 0))})
   `zipmap             (fn-scheme {:kinds [:* :*]
                                   :args  [(vec-type 0)
                                           (vec-type 1)]
                                   :ret   (map-type 0 1)})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Tuple
   `->tuple2           (fn-scheme {:kinds [:* :*]
                                   :args  [0 1]
                                   :ret   (tuple-type [0 1])})
   `left               (fn-scheme {:kinds [:* :*]
                                   :args  [(tuple-type [0 1])]
                                   :ret   0})
   `right              (fn-scheme {:kinds [:* :*]
                                   :args  [(tuple-type [0 1])]
                                   :ret   1})
   `assoc-left         (fn-scheme {:kinds [:* :*]
                                   :args  [(tuple-type [0 1]) 0]
                                   :ret   (tuple-type [0 1])})
   `assoc-right        (fn-scheme {:kinds [:* :*]
                                   :args  [(tuple-type [0 1]) 1]
                                   :ret   (tuple-type [0 1])})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Set
   `->set1             (fn-scheme {:kinds [:*]
                                   :args  [0]
                                   :ret   (set-type 0)})
   `->set2             (fn-scheme {:kinds [:*]
                                   :args  [0 0]
                                   :ret   (set-type 0)})
   `->set3             (fn-scheme {:kinds [:*]
                                   :args  [0 0 0]
                                   :ret   (set-type 0)})
   `vec->set           (fn-scheme {:kinds [:*]
                                   :args  [(vec-type 0)]
                                   :ret   (set-type 0)})
   `map->set           (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1)]
                                   :ret   (set-type (tuple-type [0 1]))})
   `set-union          (fn-scheme {:kinds [:*]
                                   :args  [(set-type 0)
                                           (set-type 0)]
                                   :ret   (set-type 0)})
   `set/difference     (fn-scheme {:kinds [:*]
                                   :args  [(set-type 0)
                                           (set-type 0)]
                                   :ret   (set-type 0)})
   `set/intersection   (fn-scheme {:kinds [:*]
                                   :args  [(set-type 0)
                                           (set-type 0)]
                                   :ret   (set-type 0)})
   `set/subset?        (fn-scheme {:kinds [:*]
                                   :args  [(set-type 0)
                                           (set-type 0)]
                                   :ret   BOOL})
   `set/superset?      (fn-scheme {:kinds [:*]
                                   :args  [(set-type 0)
                                           (set-type 0)]
                                   :ret   BOOL})
   `conj-set           (fn-scheme {:kinds [:*]
                                   :args  [(set-type 0)
                                           0]
                                   :ret   (set-type 0)})
   `disj               (fn-scheme {:kinds [:*]
                                   :args  [(set-type 0)
                                           0]
                                   :ret   (set-type 0)})
   `set-contains?      (fn-scheme {:kinds [:*]
                                   :args  [(set-type 0)
                                           0]
                                   :ret   BOOL})
   `map-set            (fn-scheme {:kinds [:* :*]
                                   :args  [(fn-type [0] 1)
                                           (set-type 0)]
                                   :ret   (set-type 1)})
   `filter-set         (fn-scheme {:kinds [:*]
                                   :args  [(fn-type [0] BOOL)
                                           (set-type 0)]
                                   :ret   (set-type 0)})
   `reduce-set         (fn-scheme {:kinds [:*]
                                   :args  [(fn-type [0 0] 0)
                                           (set-type 0)]
                                   :ret   0})
   `fold-set           (fn-scheme {:kinds [:* :*]
                                   :args  [(fn-type [1 0] 1)
                                           1
                                           (set-type 0)]
                                   :ret   1})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Map
   `->map1             (fn-scheme {:kinds [:* :*]
                                   :args  [0 1]
                                   :ret   (map-type 0 1)})
   `->map2             (fn-scheme {:kinds [:* :*]
                                   :args  [0 1 0 1]
                                   :ret   (map-type 0 1)})
   `->map3             (fn-scheme {:kinds [:* :*]
                                   :args  [0 1 0 1 0 1]
                                   :ret   (map-type 0 1)})
   `vec->map           (fn-scheme {:kinds [:* :*]
                                   :args  [(vec-type (tuple-type [0 1]))]
                                   :ret   (map-type 0 1)})
   `set->map           (fn-scheme {:kinds [:* :*]
                                   :args  [(set-type (tuple-type [0 1]))]
                                   :ret   (map-type 0 1)})
   `get                (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1) 0]
                                   :ret   1})
   `get-or-else        (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1) 0 1]
                                   :ret   1})
   `safe-assoc         (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1)
                                           0
                                           1]
                                   :ret   (map-type 0 1)})
   `update             (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1)
                                           0
                                           (fn-type [1] 1)]
                                   :ret   (map-type 0 1)})
   `conj-map           (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1)
                                           (tuple-type [0 1])]
                                   :ret   (map-type 0 1)})
   `map-contains?      (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1) 0]
                                   :ret   BOOL})
   `keys-vec           (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1)]
                                   :ret   (vec-type 0)})
   `keys-set           (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1)]
                                   :ret   (set-type 0)})
   `vals-vec           (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1)]
                                   :ret   (vec-type 1)})
   `safe-merge         (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1)
                                           (map-type 0 1)]
                                   :ret   (map-type 0 1)})
   `count-map          (fn-scheme {:kinds [:* :*]
                                   :args  [(map-type 0 1)]
                                   :ret   INT})
   `mapv-map           (fn-scheme {:kinds [:* :* :*]
                                   :args  [(fn-type [(tuple-type [0 1])] 2)
                                           (map-type 0 1)]
                                   :ret   (vec-type 2)})
   `filter-map         (fn-scheme {:kinds [:* :*]
                                   :args  [(fn-type [(tuple-type [0 1])] BOOL)
                                           (map-type 0 1)]
                                   :ret   (map-type 0 1)})
   `reduce-map         (fn-scheme {:kinds [:* :*]
                                   :args  [(fn-type [(tuple-type [0 1])
                                                     (tuple-type [0 1])]
                                                    (tuple-type [0 1]))
                                           (map-type 0 1)]
                                   :ret   (tuple-type [0 1])})
   `fold-map           (fn-scheme {:kinds [:* :* :*]
                                   :args  [(fn-type [2 (tuple-type [0 1])] 2)
                                           2
                                           (map-type 0 1)]
                                   :ret   2})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Printing & Side Effects
   ;; Use unqualified symbol of `do` because it is a special symbol.
   'do                 (fn-scheme {:kinds [:*]
                                   :args  [NIL 0]
                                   :ret   0})
   `print              (fn-scheme {:kinds [:*]
                                   :args  [0]
                                   :ret   NIL})
   `println            (fn-scheme {:kinds [:*]
                                   :args  [0]
                                   :ret   NIL})})
