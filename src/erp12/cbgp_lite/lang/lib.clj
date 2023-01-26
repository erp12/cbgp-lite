(ns erp12.cbgp-lite.lang.lib
  (:refer-clojure :exclude [and or vector-of])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [erp12.cbgp-lite.lang.schema :as schema]))

;; @todo What do do about nil?
;; first, last, etc. return nil on empty collections.
;; inc, +, etc. throw on nil.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions

(defn mapcatv
  [f coll]
  (vec (mapcat f coll)))

(defn removev
  [pred coll]
  (vec (remove pred coll)))

(defn int->char
  [i]
  (char (mod i 128)))

(defn string-concat
  [s1 s2]
  (apply str (concat s1 s2)))

(defn append-str
  [s to-append]
  (str s to-append))

(defn take-str
  [n s]
  (apply str (take n s)))

(defn rest-str
  [s]
  (apply str (rest s)))

(defn butlast-str
  [s]
  (apply str (butlast s)))

(def ^:private regex-char-esc-smap
  (let [esc-chars "()*&^%$#!"]
    (zipmap esc-chars
            (map #(str "\\" %) esc-chars))))

(defn- str-to-pattern
  [string]
  (->> string
       (replace regex-char-esc-smap)
       str/join
       re-pattern))

(defn split-str
  [s split-on]
  (str/split s (re-pattern (str-to-pattern split-on))))

(defn split-str-on-char
  [s c]
  (split-str s (str c)))

(defn split-str-on-ws
  [s]
  (str/split (str/trim s) #"\s+"))

(defn substring?
  [s sub]
  (<= 0 (.indexOf s sub)))

(defn filter-str
  [pred s]
  (str/join (filter pred s)))

(defn contains-char?
  [s c]
  (<= 0 (.indexOf s (str c))))

(defn occurrences-of-char
  [s c]
  (count (filter #{c} s)))

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

(defn concatv
  [v1 v2]
  (vec (concat v1 v2)))

(defn takev
  [n v]
  (vec (take n v)))

(defn restv
  [v]
  (vec (rest v)))

(defn butlastv
  [v]
  (vec (butlast v)))

(defn reversev
  [v]
  (vec (reverse v)))

(defn in?
  [vtr el]
  (<= 0 (.indexOf vtr el)))

(defn index-of
  [vtr el]
  (.indexOf vtr el))

(defn occurrences-of
  [vtr el]
  (count (filter #{el} vtr)))

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

(defn safe-subs
  [s start end]
  (let [start (min (count s) (max 0 start))
        end (min (count s) (max start end))]
    (subs s start end)))

(defn safe-subvec
  [vtr start end]
  (let [start (min (count vtr) (max 0 start))
        end (min (count vtr) (max start end))]
    (subvec vtr start end)))

(defn safe-assoc
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

(defn whitespace?
  [^Character c]
  (Character/isWhitespace c))

(defn digit?
  [^Character c]
  (Character/isDigit c))

(defn letter?
  [^Character c]
  (Character/isLetter c))

(defn and
  [a b]
  (core/and a b))

(defn or
  [a b]
  (core/or a b))

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

(defn simple-fn
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

(def type-env
  {;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Conditional Control Flow
   'if                  {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [BOOLEAN (s-var 'a) (s-var 'a)]
                                            (s-var 'a))}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Higher Order Functions
   'mapv                {:type   :scheme
                         :s-vars ['a 'b]
                         :body   (simple-fn [(simple-fn [(s-var 'a)] (s-var 'b))
                                             (vector-of (s-var 'a))]
                                            (vector-of (s-var 'b)))}
   'mapv2               {:type   :scheme
                         :s-vars ['a1 'a2 'b]
                         :body   (simple-fn [(simple-fn [(s-var 'a1) (s-var 'a2)] (s-var 'b))
                                             (vector-of (s-var 'a1))
                                             (vector-of (s-var 'a2))]
                                            (vector-of (s-var 'b)))}
   'filterv             {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(simple-fn [(s-var 'a)] BOOLEAN)
                                             (vector-of (s-var 'a))]
                                            (vector-of (s-var 'a)))}
   `removev             {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(simple-fn [(s-var 'a)] BOOLEAN)
                                             (vector-of (s-var 'a))]
                                            (vector-of (s-var 'a)))}
   `mapcatv             {:type   :scheme
                         :s-vars ['a 'b]
                         :body   (simple-fn [(simple-fn [(s-var 'a)] (vector-of (s-var 'b)))
                                             (vector-of (s-var 'a))]
                                            (vector-of (s-var 'b)))}
   'reduce              {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(simple-fn [(s-var 'a) (s-var 'a)] (s-var 'a))
                                             (vector-of (s-var 'a))]
                                            (s-var 'a))}
   'fold                {:type   :scheme
                         :s-vars ['a 'b]
                         :body   (simple-fn [(simple-fn [(s-var 'b) (s-var 'a)] (s-var 'b))
                                             (s-var 'b)
                                             (vector-of (s-var 'a))]
                                            (s-var 'b))}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Common
   '=                   {:type   :scheme
                         :s-vars ['a 'b]
                         :body   (simple-fn [(s-var 'a) (s-var 'b)] BOOLEAN)}
   'not=                {:type   :scheme
                         :s-vars ['a 'b]
                         :body   (simple-fn [(s-var 'a) (s-var 'b)] BOOLEAN)}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Numeric
   'int-add             (binary-transform INT)
   'int-sub             (binary-transform INT)
   'int-mult            (binary-transform INT)
   'int-div             (simple-fn [INT INT] DOUBLE)
   'int-quot            (binary-transform INT)
   'int-mod             (binary-transform INT)
   'int-inc             (unary-transform INT)
   'int-dec             (unary-transform INT)
   'int-lt              (binary-pred INT)
   'int-gt              (binary-pred INT)
   'int-le              (binary-pred INT)
   'int-ge              (binary-pred INT)
   'double-add          (binary-transform DOUBLE)
   'double-sub          (binary-transform DOUBLE)
   'double-mult         (binary-transform DOUBLE)
   'double-div          (binary-transform DOUBLE)
   'double-quot         (binary-transform DOUBLE)
   'double-mod          (binary-transform DOUBLE)
   'double-inc          (unary-transform DOUBLE)
   'double-dec          (unary-transform DOUBLE)
   'double-lt           (binary-pred DOUBLE)
   'double-gt           (binary-pred DOUBLE)
   'double-le           (binary-pred DOUBLE)
   'double-ge           (binary-pred DOUBLE)
   'int                 (simple-fn [DOUBLE] INT)
   'double              (simple-fn [INT] DOUBLE)
   'char->int           (simple-fn [CHAR] INT)
   'min-int             (binary-transform INT)
   'min-double          (binary-transform DOUBLE)
   'max-int             (binary-transform INT)
   'max-double          (binary-transform DOUBLE)
   `sin                 (unary-transform DOUBLE)
   `cos                 (unary-transform DOUBLE)
   `tan                 (unary-transform DOUBLE)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Text
   'str                 {:type   :scheme
                         :s-vars ['t]
                         :body   (simple-fn [(s-var 't)] STRING)}
   `int->char           (simple-fn [INT] CHAR)
   `whitespace?         (unary-pred CHAR)
   `digit?              (unary-pred CHAR)
   `letter?             (unary-pred CHAR)
   `string-concat       (simple-fn [STRING STRING] STRING)
   `append-str          (simple-fn [STRING CHAR] STRING)
   `take-str            (simple-fn [INT STRING] STRING)
   `safe-subs           (simple-fn [STRING INT INT] STRING)
   `filter-str          (simple-fn [(simple-fn [CHAR] BOOLEAN) STRING] STRING)
   'first-str           (simple-fn [STRING] CHAR)
   'last-str            (simple-fn [STRING] CHAR)
   `rest-str            (unary-transform STRING)
   `butlast-str         (unary-transform STRING)
   'nth-str             (simple-fn [STRING INT] CHAR)
   'length              (simple-fn [STRING] INT)
   'map-str             {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(simple-fn [CHAR] (s-var 'a))
                                             STRING]
                                            (vector-of (s-var 'a)))}
   `str/reverse         (unary-transform STRING)
   'string->chars       (simple-fn [STRING] (vector-of CHAR))
   `split-str           (simple-fn [STRING STRING] (vector-of STRING))
   `split-str-on-char   (simple-fn [STRING CHAR] (vector-of STRING))
   `split-str-on-ws     (simple-fn [STRING] (vector-of STRING))
   'empty-str?          (unary-pred STRING)
   `substring?          (binary-pred STRING)
   `contains-char?      (simple-fn [STRING CHAR] BOOLEAN)
   'index-of-char       (simple-fn [STRING CHAR] INT)
   'index-of-str        (simple-fn [STRING STRING] INT)
   `occurrences-of-char (simple-fn [STRING CHAR] INT)
   `str/replace         (simple-fn [STRING STRING STRING] STRING)
   `str/replace-first   (simple-fn [STRING STRING STRING] STRING)
   `replace-char        (simple-fn [STRING CHAR CHAR] STRING)
   `replace-first-char  (simple-fn [STRING CHAR CHAR] STRING)
   `remove-char         (simple-fn [STRING CHAR] STRING)
   `set-char            (simple-fn [STRING INT CHAR] STRING)
   `str/join            {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a))] STRING)}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Boolean
   `and                 (binary-transform BOOLEAN)
   `or                  (binary-transform BOOLEAN)
   'not                 (unary-transform BOOLEAN)
   'zero-int?           (simple-fn [INT] BOOLEAN)
   'zero-double?        (simple-fn [DOUBLE] BOOLEAN)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Vector
   `concatv             {:type   :scheme
                         :s-vars ['a]
                         :body   (binary-transform (vector-of (s-var 'a)))}
   'conj                {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a)) (s-var 'a)] (vector-of (s-var 'a)))}
   `takev               {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [INT (vector-of (s-var 'a))]
                                            (vector-of (s-var 'a)))}
   `safe-subvec         {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a)) INT INT]
                                            (vector-of (s-var 'a)))}
   'first               {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a))] (s-var 'a))}
   'last                {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a))] (s-var 'a))}
   `restv               {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a))] (vector-of (s-var 'a)))}
   `butlastv            {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a))] (vector-of (s-var 'a)))}
   `safe-nth            {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a)) INT] (s-var 'a))}
   'count               {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a))] INT)}
   `reversev            {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a))] (vector-of (s-var 'a)))}
   'empty?              {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a))] BOOLEAN)}
   `in?                 {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a)) (s-var 'a)] BOOLEAN)}
   `index-of            {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a)) (s-var 'a)] INT)}
   `occurrences-of      {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a)) (s-var 'a)] INT)}
   `safe-assoc          {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a))
                                             INT
                                             (s-var 'a)]
                                            (vector-of (s-var 'a)))}
   `replacev            {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a))
                                             (s-var 'a)
                                             (s-var 'a)]
                                            (vector-of (s-var 'a)))}
   `replacev-first      {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a))
                                             (s-var 'a)
                                             (s-var 'a)]
                                            (vector-of (s-var 'a)))}
   `remove-element      {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(vector-of (s-var 'a)) (s-var 'a)]
                                            (vector-of (s-var 'a)))}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Printing & Side Effects
   'do2                 {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [NIL (s-var 'a)] (s-var 'a))}
   'do3                 {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [NIL NIL (s-var 'a)] (s-var 'a))}
   'print               {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(s-var 'a)] NIL)}
   'println             {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn [(s-var 'a)] NIL)}
   })

(def dealiases
  '{char->int     int
    do2           do
    do3           do
    empty-str?    empty?
    first-str     first
    double-add    +
    double-dec    dec
    double-div    erp12.cbgp-lite.lang.lib/safe-div
    double-ge     >=
    double-gt     >
    double-inc    inc
    double-le     <=
    double-lt     <
    double-mod    erp12.cbgp-lite.lang.lib/safe-mod
    double-mult   *
    double-sub    -
    double-quot   erp12.cbgp-lite.lang.lib/safe-quot
    fold          reduce
    index-of-char clojure.string/index-of
    index-of-str  clojure.string/index-of
    int-add       +
    int-dec       dec
    int-div       erp12.cbgp-lite.lang.lib/safe-div
    int-ge        >=
    int-gt        >
    int-inc       inc
    int-le        <=
    int-lt        <
    int-mod       erp12.cbgp-lite.lang.lib/safe-mod
    int-mult      *
    int-sub       -
    int-quot      erp12.cbgp-lite.lang.lib/safe-quot
    last-str      last
    length        count
    map-str       mapv
    mapv2         map
    max-double    max
    max-int       max
    min-double    min
    min-int       min
    nth-str       erp12.cbgp-lite.lang.lib/safe-nth
    string->chars vec
    zero-double?  zero?
    zero-int?     zero?})

(def macros
  #{'if 'do2 'do3})

(defn lib-for-types
  [types]
  (->> type-env
       (filter (fn [[_ typ]]
                 (core/or (= (:type typ) :scheme)
                          (some #(schema/occurs? % typ) types))))
       (into {})))
