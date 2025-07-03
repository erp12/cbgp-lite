(ns erp12.cbgp-lite.lang.decompile
  (:require [clojure.tools.analyzer.jvm :as ana.jvm]
            [erp12.cbgp-lite.lang.ast :as ast]
            [erp12.cbgp-lite.lang.compile :as co]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.search.plushy :as pl]
            [erp12.cbgp-lite.task :as tsk]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Compilation testing

(defn compile-debugging
  ([genome ret-type]
   (compile-debugging genome ret-type false))
  ([genome ret-type verbose]
   (let [_ (when verbose (println "PLUSHY:" genome))
         push (pl/plushy->push genome)
         _ (when verbose (println "PUSH:" push))
         ast (::co/ast (co/push->ast {:push push
                                      :ret-type ret-type
                                      :type-env lib/type-env}))
         _ (when verbose (println "AST:" ast))
         form (ast/ast->form ast)
         _ (when verbose (println "FORM:" form))
         func (ast/form->fn [] form)]
     (func))))

(defn compile-debugging2
  ([genome task]
   (compile-debugging2 genome task []))

  ([genome task args]
   (compile-debugging2 genome task args false))

  ([genome task args verbose]
   (let [enhanced-task (tsk/enhance-task task)
         locals (:arg-symbols enhanced-task)
         _ (when verbose (println "PLUSHY:" genome))
         push (pl/plushy->push genome)
         _ (when verbose (println "PUSH:" push))
         ast (::co/ast (co/push->ast
                        (assoc
                         enhanced-task
                         :locals locals
                         :push push
                         :type-env (merge (:type-env enhanced-task)
                                          lib/type-env))))
         _ (when verbose (println "AST:" ast))
         form (ast/ast->form ast)
         _ (when verbose (println "FORM:" form))
         func (ast/form->fn locals form)]
     (apply func args))))

(comment

  ;;; Test for Count Odds problem
  (let [task {:input->type {'input1 {:type :vector :child {:type 'int?}}}
              :ret-type {:type 'int?}}
        genome (list {:gene :local ;; arg to the function (vec)
                      :idx 0}
                     {:gene :fn    ;; create an anon fn
                      :arg-types [lib/INT]
                      :ret-type lib/BOOLEAN}
                     {:gene :lit   ;; 2 (in anon fn)
                      :val 2
                      :type {:type 'int?}}
                     {:gene :local ;; arg to the anon fn (int)
                      :idx 1}
                     {:gene :var   ;; mod in anon fn
                      :name 'int-mod}
                     {:gene :apply} ;; apply mod
                     {:gene :lit ;;; 1
                      :val 1
                      :type {:type 'int?}}
                     {:gene :var  ;; = (of modded input and 1)
                      :name '=}
                     {:gene :apply} ;; apply =
                     {:gene :close} ;; end anon fn
                     {:gene :var
                      :name 'filterv} ;; call filter on anon fn and vector input
                     {:gene :apply} ;; apply filter
                     {:gene :var
                      :name 'count-vec} ;; count the filtered vector
                     {:gene :apply})] ;; apply count

    (compile-debugging2 genome
                        task
                        [[8 3 2 5 7 0 11]]
                        true))
;;; Test for  Smallest problem 
  (let [task {:input->type {'input1 {:type 'int?}
                            'input2 {:type 'int?}
                            'input3 {:type 'int?}
                            'input4 {:type 'int?}}
              :ret-type {:type 'int?}}
        genome [{:gene :local
                 :idx 0}
                {:gene :local
                 :idx 1}
                {:gene :var
                 :name `lib/min'}
                {:gene :apply}
                {:gene :local
                 :idx 2}
                {:gene :var
                 :name `lib/min'}
                {:gene :apply}
                {:gene :local
                 :idx 3}
                {:gene :var
                 :name `lib/min'}
                {:gene :apply}]]
    (compile-debugging2 genome
                        task
                        [5 6 -33 9]
                        true))

  ;; Test for Number IO
  (let [task {:input->type {'input1 {:type 'double?}
                            'input2 {:type 'int?}}
              :ret-type {:type 'string?}}
        genome (list {:gene :local
                      :idx 1}
                     {:gene :var
                      :name 'double}
                     {:gene :apply}
                     {:gene :local
                      :idx 0}
                     {:gene :var
                      :name 'double-add}
                     {:gene :apply}
                     {:gene :var
                      :name 'str}
                     {:gene :apply})]
    (compile-debugging2 genome
                        task
                        [100.23 33]
                        true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Below here is work on decompiling

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Broken instructions
;; < is broken for any number of arguments != 2
;; / with more than 2 arguments

;;;; Broken because they're macros
;;  'and `lib/and
;;  'or `lib/or

(def fns-llm-won't-use
  [`lib/int-ceil
   `lib/int-floor
   `lib/safe-log2
   `lib/double-square
   `lib/int-square
   `lib/int-pow
   'zero-double?])

(def work-without-change
  [;; FP
   'comp
   'partial

   ;; Common/Misc
   'not
   'not=
   'if
   'print
   'println

   ;; Numeric
   'mod
   'inc
   'dec
   'abs

   ;; Text

   ;; Collections
   'count
   'vec
   'set
   'first
   'last
   'empty?
   'contains?
   'assoc
   'merge
   'disj
   'get
   'update])

(def ast-aliasing
  {;; Common
   'equiv '=
   'lt `lib/<'
   'lte `lib/<='
   'gt `lib/>'
   'gte `lib/>='
   'max `lib/max'
   'min `lib/min'

   ;; Numeric
   'add '+
   'sub '-
   'multiply '*
   'quotient 'quot
   'divide '/
   'neg `lib/neg ; --> arity; minus w/ one arg
   'pow `lib/pow
   ; square (skip)
   'intCast 'int
   'doubleCast 'double
   'sqrt `lib/safe-sqrt
   'sin `lib/sin
   'cos `lib/cos
   'tan `lib/tan
   'asin `lib/safe-asin
   'acos `lib/safe-acos
   'atan `lib/atan
   ; safe-log2 (skip)
   'log10 `lib/safe-log10
   'ceil `lib/ceil
   'floor `lib/floor
   'isZero 'zero-int?

   ;; Text
   ; `lib/join, 'str-join-sep --> arity; str-join-sep w/ 2 args
   ; 'str, 'append-str --> arity; append-str w/ 2 args
   'charCast `lib/int->char
   'isWhitespace `lib/whitespace?
   'isDigit `lib/digit?
   'isLetter `lib/letter?
   ; split-str-on-ws, split-str --> arity; split-str w/ 2 args
   ; `lib/set-char (doesn't exist in clojure?)
   'capitalize  `str/capitalize
   'upper-case  `str/upper-case
   'lower-case  `str/lower-case
   'toUpperCase  `lib/char-upper
   'toLowerCase  `lib/char-lower

   ;; Boolean
   ; 'and__5600__auto__  `lib/and
   ; 'or__5602__auto__  `lib/or
   
   ;; Collections
   ; 'mapv
   ; `lib/map2v --> arity; mapv w/ 2 args
   ; `lib/->map --> (?) arity
   'concat `lib/concat'
   'conj `lib/conj'
   'rest `lib/rest'
   'butlast `lib/butlast'
   'includes? `lib/in?
   'indexOf `lib/index-of
   'index-of `lib/index-of
   'filter `lib/filter'
   'remove `lib/remove'
   ; `lib/remove-element --> arity; remove w/ 2 args
   ; 'reduce, 'fold --> arity
   'mapcat `lib/mapcat'

   ;; Text/Vec
   'nth `lib/safe-nth
   'replace `lib/replace'
   'replace-first `lib/replace-first'
   'take `lib/take'
   'reverse `lib/reverse'
   'sort `lib/sort'
   'subs `lib/safe-sub-coll'
   'subvec `lib/safe-sub-coll'

   ;; Vector
   ; ->vector (arity)
   ; 'nth-or-else (arity; 'nth)
   ; `lib/occurrences-of (doesn't exist in clojure?)
   ; 'safe-assoc-nth (typed; assoc on a vec)
   ; 'range (arity)
   'map-indexed `lib/mapv-indexed
   'distinct `lib/distinctv
   'sort-by `lib/sortv-by
   'group-by 'group-by
   'zipmap 'zipmap

   ;; Set
   ; ->set (arity)
   'union `set/union
   'difference `set/difference
   'intersection `set/intersection
   'subset? `set/subset?
   'superset? `set/superset?
   ; `lib/map-set (may delete; keep mapv-set)
   
   ;; Map
   ; ->map (arity)
   ; 'get-or-else (arity; 'get w/ 3 args)
   'keys `lib/keys-vec
   ; `lib/keys-set (doesn't exist in clojure?)
   'vals `lib/vals-vec})

(def ast-number-aliasing
  {
  ;;  'add "add"
  ;;  'sub "sub"
  ;;  'multiply "mult"
  ;;  'divide "div"
  ;;  'quotient "quot"
  ;;  'mod "mod"
  ;;  'inc "inc"
  ;;  'dec "dec"
  ;;  'neg "neg" ; minus w/ one arg
  ;;  'abs "abs"
  ;;  'intCast 'int
   })
   ; 'intCast 'char->int

(def ast-str-vec-aliasing
  {;'first "first"
   ;'last "last" 
   'nth 'nth
   ;'empty? "empty"
   })

  ;; Same Namespace issues as in ast-number-aliasing
  ;; Make a new dictionary for namespace qualified (ns-q)
  ;; symbols that are type dependent,  
  ;; because ns-q vector symbols append v
  ;; whereas other non-ns-q append nothing or vec 
  ;; 'rest "`lib/rest"

(def ast-collection-aliasing
  {;'count "count"
   })
  ;;  'reduce "reduce"
  ;;  'fold "fold"

(def ast-arity-aliasing
  ;; to include in this map:
  ; - neg
  ; `lib/join 'str-join-sep
  ; 'str 'append-str
  ; split-str-on-ws split-str
  ; mapv map2v
  ; ->map ->map1 ->map2 ->map3
  ; ->vector1 ->vector2 ->vector3
  ; ->set1 ->set2 ->set3 
  ; range1 range2 range3
  ; remove remove-element
  ; reduce fold
  ; nth nth-or-else
  ; get get-or-else


  {'str {1 'str
         2 `lib/concat-str
         :default `lib/concat-str}
   'minus {1 'neg
           2 'sub
           :default 'sub}
   ; Does not work on strings
   'nth {2 `lib/safe-nth
         3 'nth-or-else}

   ; TO-DO:
  ;;  'reduce {2 'reduce
  ;;           3 'fold}

   'vector {1 '->vector1
            2 '->vector2
            3 '->vector3
            :default '->vector1}
   ; TO-DO: add check for vec->set and map->set
   'hash-set {1 '->set1
              2 '->set2
              3 '->set3
              :default '->set1}
   ; TO-DO: add check for vec->map and set->map
   'hash-map {2 '->map1
              4 '->map2
              6 '->map3
              :default '->map1}
   'range {1 'range1
           2 'range2
           3 'range3
           :default 'range1}})

(def ast-namespace-qualified-type-aliasing
  {'rest "rest"})
  ;;  'concat "concat"

(defn find-local
  "Takes a map or vec and recursively looks through it to find a map
   with a key of :op and value of :local"
  [map-or-vec]
  ;; (println (:tag map-or-vec))
  (cond
    (and (map? map-or-vec)
         (= (:op map-or-vec) :local))
    map-or-vec

    (map? map-or-vec)
    (first (filter #(not (nil? %))
                   (map find-local
                        (vals map-or-vec))))

    (vector? map-or-vec)
    (first (filter #(not (nil? %))
                   (map find-local
                        map-or-vec)))

    :else
    nil))

(defn get-fn-symbol
  "Finds the CBGP function name for this ast-fn-name"
  [ast-fn-name tag args task]
  ;; (println "Firstp Task: " task ast-fn-name)
  (cond
    ;; Because of the phrasing, this needs to be hard coded
    (= ast-fn-name 'intCast)
    (if (= (str (:tag (first args))) "char")
      'char->int
      'int)

    ;; functions with multiple arities to support
    (contains? ast-arity-aliasing ast-fn-name)
    (let [arity-map (get ast-arity-aliasing ast-fn-name)
          fn-symbol (get arity-map
                         (count args)
                         (get arity-map :default))]
      (if (contains? ast-number-aliasing fn-symbol)
        (get-fn-symbol fn-symbol tag args task)
        fn-symbol))

    ;; numbers
    (contains? ast-number-aliasing ast-fn-name)
    (symbol (str (cond
                   (= (str tag) "double") "double-"
                   (= (str tag) "long") "int-"
                   (= tag java.lang.Long) "int-"
                   (= tag java.lang.Integer) "int-"
                   (= ast-fn-name 'div) "int-"

                   ;; Handle local making it so we don't know the type
                   ;; Note; defaults to int? even when the type isn't
                   ;; double? or int? This may occasionally break things
                   :else
                   (if (= 'double?
                          (:type (get (:input->type task)
                                      (:form (find-local args)))))
                     "double-"
                     "int-"))
                 (get ast-number-aliasing ast-fn-name)))

    ;; Vector stuff
    (contains? ast-str-vec-aliasing ast-fn-name)
    (symbol (str (get ast-str-vec-aliasing ast-fn-name)
                 (cond
                   (= (:tag (first args)) java.lang.String) "-str"
                   (= (:tag (first args)) clojure.lang.APersistentVector) ""
                   :else
                   (if (= 'string?
                          (:type (get (:input->type task)
                                      (:form (find-local args)))))
                     "-str"
                     ""))

                 (if (= 'empty? ast-fn-name)
                   "?"
                   "")))

    ;;Vector-set-map
    (contains? ast-collection-aliasing ast-fn-name)
    (symbol (let [symb (str (get ast-collection-aliasing ast-fn-name)
                            (cond
                              (string? (:val (first args)))
                              "-str"
                              (set? (:val (first args)))
                              "-set"
                              (map? (:val (first args)))
                              "-map"
                              (vector? (:val (first args)))
                              "-vec"
                              :else
                              (cond
                                (= 'string?
                                   (:type (get (:input->type task)
                                               (:form (find-local args)))))
                                "-str"
                                (= :set
                                   (:type (get (:input->type task)
                                               (:form (find-local args)))))
                                "-set"
                                (= :map
                                   (:type (get (:input->type task)
                                               (:form (find-local args)))))
                                "-map"
                                (= :vector
                                   (:type (get (:input->type task)
                                               (:form (find-local args)))))
                                "-vec"
                                :else
                                "-BAD")))]
              ;; I didn't want to do this but CBGP naming 
              ;; "conventions" forced my hand
              (if (= symb "count-str")
                "length"
                symb)))

    ;; rest only right now?
    (contains? ast-namespace-qualified-type-aliasing ast-fn-name)
    (symbol "erp12.cbgp-lite.lang.lib" (cond (get ast-namespace-qualified-type-aliasing ast-fn-name)
                                             (cond
                                               (= (str tag) "double")
                                               (str "double-" (get ast-namespace-qualified-type-aliasing ast-fn-name))

                                               (= (:tag (first args)) java.lang.String)
                                               (str (get ast-namespace-qualified-type-aliasing ast-fn-name) "-str")

                                               (= (:tag (first args)) clojure.lang.PersistentVector)
                                               (str (get ast-namespace-qualified-type-aliasing ast-fn-name) "v")

                                               :else (str "int-" (get ast-namespace-qualified-type-aliasing ast-fn-name)))))

    ;; main aliasing
    (contains? ast-aliasing ast-fn-name)
    (get ast-aliasing ast-fn-name)

    :else ast-fn-name))

(defn find-type
  "Returns the type of val in the given ast"
  [val ast]
  (cond
    ;; Ground types
    (integer? val) {:type 'int?}
    (number? val) {:type 'double?}
    (boolean? val) {:type 'boolean?}
    (string? val) {:type 'string?}
    (char? val) {:type 'char?}
    (keyword? val) {:type 'keyword?}
    (symbol? val) {:type 'symbol?}
    (nil? val) {:type 'nil?}

    ;; Vectors and sets
    (or (vector? val)
        (set? val))
    (let [child-type (if (empty? val)
                       (lib/s-var 'T)
                       (find-type (first val)
                                  ast))]
      {:type (:type ast) :child child-type})

    ;; Maps
    (map? val)
    (let [key-type (if (empty? val)
                     (lib/s-var 'T)
                     (find-type (first (first val))
                                (ana.jvm/analyze (first (first val)))))
          val-type (if (empty? val)
                     (lib/s-var 'S)
                     (find-type (second (first val))
                                (ana.jvm/analyze (second (first val)))))]
      {:type :map-of :key key-type :value val-type})

    :else (throw (Exception.
                  (str "AST contains a type that shouldn't be possible: "
                       ast)))))

(defn decompile-ast
  "Decompiles AST into a CBGP genome."
  ([ast] (decompile-ast ast {}))
  ([{:keys [op val tag args children] :as ast} task]
  ;;  (println "Decomp Task: " task)
   (cond
    ;; Handle constants
     (= :const op)
     (list {:gene :lit
            :val val
            :type (find-type val ast)})

     ;; Handle locals
     (= :local op)
     (list {:gene :local
            :idx (:arg-id ast)})

    ;; Handle static method or invoke
     (or (= op :static-call)
         (= op :invoke))
     (let [ast-fn-name (if (= op :static-call)
                         (:method ast)
                         (-> ast :fn :form))
           raw-decompiled-args (map #(decompile-ast % task) args)
           decompiled-args (flatten (reverse raw-decompiled-args))]
       (concat decompiled-args
               (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)}
                     {:gene :apply})))

    ;; Handle quote for lists; translate into vector
     (= op :quote)
     (let [the-vector (vec (-> ast :expr :val))]
       (list {:gene :lit
              :val the-vector
              :type (find-type the-vector (assoc ast :type :vector))}))

    ;; Handle if
     (= op :if)
     (let [ast-fn-name 'if
           raw-decompiled-args (map #(decompile-ast % task) (map ast children))
           decompiled-args (flatten (reverse raw-decompiled-args))]
       (concat decompiled-args
               (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)}
                     {:gene :apply})))

    ;; Handle anonymous function abstraction
     (= op :fn)
     nil

     (= op :def)
     (decompile-ast (-> ast
                        :init
                        :expr
                        :methods
                        first
                        :body)
                    task)

     :else
     (do
       (println "not handled yet AST op:" op)
       nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing

(comment

  (decompile-ast (ana.jvm/analyze '(and [1 2] 2)))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(nth [1.0 2.0 3.0] 10 4.04)))
                     {:type 'double?})
;;;; THESE DON'T WORK

  (decompile-ast (ana.jvm/analyze '(nth [1 2 3] 2 5)))

  (compile-debugging (decompile-ast (ana.jvm/analyze '(or true false)))
                     {:type 'boolean?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(< 4 5 8)))
                     {:type 'boolean?})

  ;;; misc stuff

  (ana.jvm/analyze '(fn [x] (+ x 1)))

  (map (fn [x] (+ x 1))
       '(3 5 1))

  (decompile-ast
   (->
    (ana.jvm/analyze '(defn help [input1] (inc input1)))
    :init
    :expr
    :methods
    first
    :body))

  (->
   (ana.jvm/analyze '(defn help [input1 input2 input3] (+ input3 input2)))
   :init
   :expr
   :methods
   first
   :body)

  (macroexpand-1 '(defn help [input1] (inc input1)))

  (decompile-ast (ana.jvm/analyze '(map inc [1 2 3])))

  (ana.jvm/analyze '(hash-map "a" 1))

  (ana.jvm/analyze 'count))

