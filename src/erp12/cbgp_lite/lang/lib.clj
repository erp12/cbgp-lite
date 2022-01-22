(ns erp12.cbgp-lite.lang.lib
  (:refer-clojure :exclude [and or])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [erp12.schema-inference.ast :as ast]
            [erp12.schema-inference.schema :as sch]))

;; @todo What do do about nil?
;; first, last, etc. return nil on empty collections.
;; inc, +, etc. throw on nil.

;; @todo Vector insert?
;; @todo chars->string
;; @todo join (vector -> string)

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

(defn iff
  [cond then else]
  (if cond then else))

(defn and
  [a b]
  (core/and a b))

(defn or
  [a b]
  (core/or a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground Schemas
;; nil? boolean? int? float? char? string? keyword?

(defn unary-transform
  [type]
  [:=> [:cat type] type])

(defn binary-transform
  [type]
  [:=> [:cat type type] type])

(defn unary-pred
  [type]
  [:=> [:cat type] boolean?])

(defn binary-pred
  [type]
  [:=> [:cat type type] boolean?])

(def library
  {;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Conditional Control Flow
   `iff                 {:s-vars ['t] :body [:=> [:cat boolean? [:s-var 't] [:s-var 't]] [:s-var 't]]}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Higher Order Functions
   'mapv                {:s-vars ['a 'b]
                         :body   [:=> [:cat [:=> [:cat [:s-var 'a]] [:s-var 'b]]
                                       [:vector [:s-var 'a]]]
                                  [:vector [:s-var 'b]]]}
   'mapv2               {:s-vars ['a1 'a2 'b]
                         :body   [:=> [:cat [:=> [:cat [:s-var 'a1] [:s-var 'a2]] [:s-var 'b]]
                                       [:vector [:s-var 'a1]]
                                       [:vector [:s-var 'a2]]]
                                  [:vector [:s-var 'b]]]}
   'filterv             {:s-vars ['a]
                         :body   [:=> [:cat [:=> [:cat [:s-var 'a]] boolean?]
                                       [:vector [:s-var 'a]]]
                                  [:vector [:s-var 'a]]]}
   `removev             {:s-vars ['a]
                         :body   [:=> [:cat [:=> [:cat [:s-var 'a]] boolean?]
                                       [:vector [:s-var 'a]]]
                                  [:vector [:s-var 'a]]]}
   `mapcatv             {:s-vars ['a 'b]
                         :body   [:=> [:cat [:=> [:cat [:s-var 'a]] [:vector [:s-var 'b]]]
                                       [:vector [:s-var 'a]]]
                                  [:vector [:s-var 'b]]]}
   'reduce              {:s-vars ['a]
                         :body   [:=> [:cat [:=> [:cat [:s-var 'a] [:s-var 'a]] [:s-var 'a]]
                                       [:vector [:s-var 'a]]]
                                  [:s-var 'a]]}
   'fold                {:s-vars ['a 'b]
                         :body   [:=> [:cat [:=> [:cat [:s-var 'b] [:s-var 'a]] [:s-var 'b]]
                                       [:s-var 'b]
                                       [:vector [:s-var 'a]]]
                                  [:s-var 'b]]}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Common
   '=                   {:s-vars ['a] :body [:=> [:cat [:s-var 'a] [:s-var 'a]] boolean?]}
   'not=                {:s-vars ['a] :body [:=> [:cat [:s-var 'a] [:s-var 'a]] boolean?]}
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Numeric
   'int-add             (binary-transform int?)
   'int-sub             (binary-transform int?)
   'int-mult            (binary-transform int?)
   'int-div             [:=> [:cat int? int?] float?]
   'int-mod             (binary-transform int?)
   'int-inc             (unary-transform int?)
   'int-dec             (unary-transform int?)
   'int-lt              (binary-pred int?)
   'int-gt              (binary-pred int?)
   'int-le              (binary-pred int?)
   'int-ge              (binary-pred int?)
   'float-add           (binary-transform float?)
   'float-sub           (binary-transform float?)
   'float-mult          (binary-transform float?)
   'float-div           (binary-transform float?)
   'float-mod           (binary-transform float?)
   'float-inc           (unary-transform float?)
   'float-dec           (unary-transform float?)
   'float-lt            (binary-pred float?)
   'float-gt            (binary-pred float?)
   'float-le            (binary-pred float?)
   'float-ge            (binary-pred float?)
   'int                 [:=> [:cat float?] int?]
   'float               [:=> [:cat int?] float?]
   'char->int           [:=> [:cat char?] int?]
   'min-int             (binary-transform int?)
   'min-float           (binary-transform float?)
   'max-int             (binary-transform int?)
   'max-float           (binary-transform float?)
   `sin                 (unary-transform float?)
   `cos                 (unary-transform float?)
   `tan                 (unary-transform float?)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Text
   'str                 {:s-vars ['t] :body [:=> [:cat [:s-var 't]] string?]}
   `int->char           [:=> [:cat int?] char?]
   `whitespace?         (unary-pred char?)
   `digit?              (unary-pred char?)
   `letter?             (unary-pred char?)
   `string-concat       [:=> [:cat string? string?] string?]
   `append-str          [:=> [:cat string? char?] string?]
   `take-str            [:=> [:cat int? string?] string?]
   `safe-subs           [:=> [:cat string? int? int?] string?]
   'first-str           [:=> [:cat string?] char?]
   'last-str            [:=> [:cat string?] char?]
   `rest-str            (unary-transform string?)
   `butlast-str         (unary-transform string?)
   'nth-str             [:=> [:cat string? int?] char?]
   'length              [:=> [:cat string?] int?]
   `str/reverse         (unary-transform string?)
   'string->chars       [:=> [:cat string?] [:vector char?]]
   `split-str           [:=> [:cat string? string?] [:vector string?]]
   `split-str-on-char   [:=> [:cat string? char?] [:vector string?]]
   `split-str-on-ws     [:=> [:cat string?] [:vector string?]]
   'empty-str?          (unary-pred string?)
   `substring?          (binary-pred string?)
   `contains-char?      [:=> [:cat string? char?] boolean?]
   `index-of-char       [:=> [:cat string? char?] int?]
   `occurrences-of-char [:=> [:cat string? char?] int?]
   `str/replace         [:=> [:cat string? string? string?] string?]
   `str/replace-first   [:=> [:cat string? string? string?] string?]
   `replace-char        [:=> [:cat string? char? char?] string?]
   `replace-first-char  [:=> [:cat string? char? char?] string?]
   `remove-char         [:=> [:cat string? char? char?] string?]
   `set-char            [:=> [:cat string? int? char?] string?]
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Boolean
   `and                 (binary-transform boolean?)
   `or                  (binary-transform boolean?)
   'not                 (unary-transform boolean?)
   'zero-int?           [:=> [:cat int?] boolean?]
   'zero-float?         [:=> [:cat float] boolean?]
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Vector
   `concatv             {:s-vars ['a] :body (binary-transform [:vector [:s-var 'a]])}
   'conj                {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]] [:s-var 'a]] [:vector [:s-var 'a]]]}
   `takev               {:s-vars ['a] :body [:=> [:cat int? [:vector [:s-var 'a]]] [:vector [:s-var 'a]]]}
   `safe-subvec         {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]] int? int?] [:vector [:s-var 'a]]]}
   'first               {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]]] [:s-var 'a]]}
   'last                {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]]] [:s-var 'a]]}
   `restv               {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]]] [:vector [:s-var 'a]]]}
   `butlastv            {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]]] [:vector [:s-var 'a]]]}
   `safe-nth            {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]] int?] [:s-var 'a]]}
   'count               {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]]] int?]}
   `reversev            {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]]] [:vector [:s-var 'a]]]}
   'empty?              {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]]] boolean?]}
   `in?                 {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]] [:s-var 'a]] boolean?]}
   `index-of            {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]] [:s-var 'a]] int?]}
   `occurrences-of      {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]] [:s-var 'a]] int?]}
   `safe-assoc          {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]] int? [:s-var 'a]] [:vector [:s-var 'a]]]}
   `replacev            {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]] [:s-var 'a] [:s-var 'a]] [:vector [:s-var 'a]]]}
   `replacev-first      {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]] [:s-var 'a] [:s-var 'a]] [:vector [:s-var 'a]]]}
   `remove-element      {:s-vars ['a] :body [:=> [:cat [:vector [:s-var 'a]] [:s-var 'a]] [:vector [:s-var 'a]]]}
   })

(def dealiases
  '{char->int     int
    empty-str?    empty?
    first-str     first
    float-add     +
    float-dec     dec
    float-div     erp12.cbgp-lite.lang.lib/safe-div
    float-ge      >=
    float-gt      >
    float-inc     inc
    float-le      <=
    float-lt      <
    float-mod     erp12.cbgp-lite.lang.lib/safe-mod
    float-mult    *
    float-sub     -
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
    max-float     max
    max-int       max
    min-float     min
    min-int       min
    nth-str       erp12.cbgp-lite.lang.lib/safe-nth
    string->chars vec
    zero-float?   zero?
    zero-int?     zero?})

(defn lib-for-types
  [types]
  (->> library
       (filter (fn [[_ typ]]
                 (core/or (sch/scheme? typ)
                          (some #(ast/occurs? % typ) types))))
       (into {})))
