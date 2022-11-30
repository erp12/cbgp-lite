(ns erp12.cbgp-lite.lang.lib
  (:refer-clojure :exclude [and or])
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

(defn split-str
  [s split-on]
  ;; @todo Should this escape regex symbols?
  (str/split s (re-pattern split-on)))

(defn split-str-on-char
  [s c]
  (split-str s (str c)))

(defn split-str-on-ws
  [s]
  (str/split (str/trim s) #"\s+"))

(defn substring?
  [s sub]
  (<= 0 (.indexOf s sub)))

(defn contains-char?
  [s c]
  (<= 0 (.indexOf s (str c))))

(defn index-of-char
  [s c]
  (.indexOf s (str c)))

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

(defn simple-fn-schema
  [args ret]
  {:type   :=>
   :input  {:type :cat :children (vec args)}
   :output ret})

(defn s-var
  [sym]
  {:type :s-var :sym sym})

(defn vector-schema
  [el]
  {:type :vector :child el})

(def type-env
  {;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Conditional Control Flow
   'if                  {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [{:type 'boolean?} (s-var 'a) (s-var 'a)]
                                                   (s-var 'a))}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Higher Order Functions
   'mapv                {:type   :scheme
                         :s-vars ['a 'b]
                         :body   (simple-fn-schema [(simple-fn-schema [(s-var 'a)] (s-var 'b))
                                                    (vector-schema (s-var 'a))]
                                                   (vector-schema (s-var 'b)))}
   'mapv2               {:type   :scheme
                         :s-vars ['a1 'a2 'b]
                         :body   (simple-fn-schema [(simple-fn-schema [(s-var 'a1) (s-var 'a2)] (s-var 'b))
                                                    (vector-schema (s-var 'a1))
                                                    (vector-schema (s-var 'a2))]
                                                   (vector-schema (s-var 'b)))}
   'filterv             {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(simple-fn-schema [(s-var 'a)] {:type 'boolean?})
                                                    (vector-schema (s-var 'a))]
                                                   (vector-schema (s-var 'a)))}
   `removev             {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(simple-fn-schema [(s-var 'a)] {:type 'boolean?})
                                                    (vector-schema (s-var 'a))]
                                                   (vector-schema (s-var 'a)))}
   `mapcatv             {:type   :scheme
                         :s-vars ['a 'b]
                         :body   (simple-fn-schema [(simple-fn-schema [(s-var 'a)] (vector-schema (s-var 'b)))
                                                    (vector-schema (s-var 'a))]
                                                   (vector-schema (s-var 'b)))}
   'reduce              {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(simple-fn-schema [(s-var 'a) (s-var 'a)] (s-var 'a))
                                                    (vector-schema (s-var 'a))]
                                                   (s-var 'a))}
   'fold                {:type   :scheme
                         :s-vars ['a 'b]
                         :body   (simple-fn-schema [(simple-fn-schema [(s-var 'b) (s-var 'a)] (s-var 'b))
                                                    (s-var 'b)
                                                    (vector-schema (s-var 'a))]
                                                   (s-var 'b))}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Common
   '=                   {:type   :scheme
                         :s-vars ['a 'b]
                         :body   (simple-fn-schema [(s-var 'a) (s-var 'b)] {:type 'boolean?})}
   'not=                {:type   :scheme
                         :s-vars ['a 'b]
                         :body   (simple-fn-schema [(s-var 'a) (s-var 'b)] {:type 'boolean?})}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Numeric
   'int-add             (binary-transform {:type 'int?})
   'int-sub             (binary-transform {:type 'int?})
   'int-mult            (binary-transform {:type 'int?})
   'int-div             (simple-fn-schema [{:type 'int?} {:type 'int?}] {:type 'double?})
   'int-mod             (binary-transform {:type 'int?})
   'int-inc             (unary-transform {:type 'int?})
   'int-dec             (unary-transform {:type 'int?})
   'int-lt              (binary-pred {:type 'int?})
   'int-gt              (binary-pred {:type 'int?})
   'int-le              (binary-pred {:type 'int?})
   'int-ge              (binary-pred {:type 'int?})
   'double-add          (binary-transform {:type 'double?})
   'double-sub          (binary-transform {:type 'double?})
   'double-mult         (binary-transform {:type 'double?})
   'double-div          (binary-transform {:type 'double?})
   'double-mod          (binary-transform {:type 'double?})
   'double-inc          (unary-transform {:type 'double?})
   'double-dec          (unary-transform {:type 'double?})
   'double-lt           (binary-pred {:type 'double?})
   'double-gt           (binary-pred {:type 'double?})
   'double-le           (binary-pred {:type 'double?})
   'double-ge           (binary-pred {:type 'double?})
   'int                 (simple-fn-schema [{:type 'double?}] {:type 'int?})
   'double              (simple-fn-schema [{:type 'int?}] {:type 'double?})
   'char->int           (simple-fn-schema [{:type 'char?}] {:type 'int?})
   'min-int             (binary-transform {:type 'int?})
   'min-double          (binary-transform {:type 'double?})
   'max-int             (binary-transform {:type 'int?})
   'max-double          (binary-transform {:type 'double?})
   `sin                 (unary-transform {:type 'double?})
   `cos                 (unary-transform {:type 'double?})
   `tan                 (unary-transform {:type 'double?})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Text
   'str                 {:type   :scheme
                         :s-vars ['t]
                         :body   (simple-fn-schema [(s-var 't)] {:type 'string?})}
   `int->char           (simple-fn-schema [{:type 'int?}] {:type 'char?})
   `whitespace?         (unary-pred {:type 'char?})
   `digit?              (unary-pred {:type 'char?})
   `letter?             (unary-pred {:type 'char?})
   `string-concat       (simple-fn-schema [{:type 'string?} {:type 'string?}] {:type 'string?})
   `append-str          (simple-fn-schema [{:type 'string?} {:type 'char?}] {:type 'string?})
   `take-str            (simple-fn-schema [{:type 'int?} {:type 'string?}] {:type 'string?})
   `safe-subs           (simple-fn-schema [{:type 'string?} {:type 'int?} {:type 'int?}] {:type 'string?})
   'first-str           (simple-fn-schema [{:type 'string?}] {:type 'char?})
   'last-str            (simple-fn-schema [{:type 'string?}] {:type 'char?})
   `rest-str            (unary-transform {:type 'string?})
   `butlast-str         (unary-transform {:type 'string?})
   'nth-str             (simple-fn-schema [{:type 'string?} {:type 'int?}] {:type 'char?})
   'length              (simple-fn-schema [{:type 'string?}] {:type 'int?})
   `str/reverse         (unary-transform {:type 'string?})
   'string->chars       (simple-fn-schema [{:type 'sing?}] (vector-schema {:type 'char?}))
   `split-str           (simple-fn-schema [{:type 'strtring?} {:type 'string?}] (vector-schema {:type 'string?}))
   `split-str-on-char   (simple-fn-schema [{:type 'string?} {:type 'char?}] (vector-schema {:type 'string?}))
   `split-str-on-ws     (simple-fn-schema [{:type 'string?}] (vector-schema {:type 'string?}))
   'empty-str?          (unary-pred {:type 'string?})
   `substring?          (binary-pred {:type 'string?})
   `contains-char?      (simple-fn-schema [{:type 'string?} {:type 'char?}] {:type 'boolean?})
   `index-of-char       (simple-fn-schema [{:type 'string?} {:type 'char?}] {:type 'int?})
   `occurrences-of-char (simple-fn-schema [{:type 'string?} {:type 'char?}] {:type 'int?})
   `str/replace         (simple-fn-schema [{:type 'string?} {:type 'string?} {:type 'string?}] {:type 'string?})
   `str/replace-first   (simple-fn-schema [{:type 'string?} {:type 'string?} {:type 'string?}] {:type 'string?})
   `replace-char        (simple-fn-schema [{:type 'string?} {:type 'char?} {:type 'char?}] {:type 'string?})
   `replace-first-char  (simple-fn-schema [{:type 'string?} {:type 'char?} {:type 'char?}] {:type 'string?})
   `remove-char         (simple-fn-schema [{:type 'string?} {:type 'char?}] {:type 'string?})
   `set-char            (simple-fn-schema [{:type 'string?} {:type 'int?} {:type 'char?}] {:type 'string?})
   `str/join            {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a))] {:type 'string?})}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Boolean
   `and                 (binary-transform {:type 'boolean?})
   `or                  (binary-transform {:type 'boolean?})
   'not                 (unary-transform {:type 'boolean?})
   'zero-int?           (simple-fn-schema [{:type 'int?}] {:type 'boolean?})
   'zero-double?        (simple-fn-schema [{:type 'double}] {:type 'boolean?})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Vector
   `concatv             {:type   :scheme
                         :s-vars ['a]
                         :body   (binary-transform (vector-schema (s-var 'a)))}
   'conj                {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a)) (s-var 'a)] (vector-schema (s-var 'a)))}
   `takev               {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [{:type 'int?} (vector-schema (s-var 'a))]
                                                   (vector-schema (s-var 'a)))}
   `safe-subvec         {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a)) {:type 'int?} {:type 'int?}]
                                                   (vector-schema (s-var 'a)))}
   'first               {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a))] (s-var 'a))}
   'last                {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a))] (s-var 'a))}
   `restv               {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a))] (vector-schema (s-var 'a)))}
   `butlastv            {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a))] (vector-schema (s-var 'a)))}
   `safe-nth            {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a)) {:type 'int?}] (s-var 'a))}
   'count               {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a))] {:type 'int?})}
   `reversev            {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a))] (vector-schema (s-var 'a)))}
   'empty?              {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a))] {:type 'boolean?})}
   `in?                 {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a)) (s-var 'a)] {:type 'boolean?})}
   `index-of            {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a)) (s-var 'a)] {:type 'int?})}
   `occurrences-of      {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a)) (s-var 'a)] {:type 'int?})}
   `safe-assoc          {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a))
                                                    {:type 'int?}
                                                    (s-var 'a)]
                                                   (vector-schema (s-var 'a)))}
   `replacev            {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a))
                                                    (s-var 'a)
                                                    (s-var 'a)]
                                                   (vector-schema (s-var 'a)))}
   `replacev-first      {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a))
                                                    (s-var 'a)
                                                    (s-var 'a)]
                                                   (vector-schema (s-var 'a)))}
   `remove-element      {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(vector-schema (s-var 'a)) (s-var 'a)]
                                                   (vector-schema (s-var 'a)))}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Printing & Side Effects
   'do2                 {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [{:type 'nil?} (s-var 'a)] (s-var 'a))}
   'do3                 {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [{:type 'nil?} {:type 'nil?} (s-var 'a)] (s-var 'a))}
   'print               {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(s-var 'a)] {:type 'nil?})}
   'println             {:type   :scheme
                         :s-vars ['a]
                         :body   (simple-fn-schema [(s-var 'a)] {:type 'nil?})}
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
    fold          reduce
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
    last-str      last
    length        count
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
