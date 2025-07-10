(ns erp12.cbgp-lite.lang.decompile
  (:require [clojure.tools.analyzer.jvm :as ana.jvm]
            [erp12.cbgp-lite.lang.ast :as ast]
            [erp12.cbgp-lite.lang.compile :as co]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.search.plushy :as pl]
            [erp12.cbgp-lite.task :as tsk]
            [taoensso.timbre :as log]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Compilation testing

(defn compile-debugging
  ([genome ret-type]
   (compile-debugging genome ret-type false))
  ([genome ret-type verbose]
   (let [_ (when verbose (println "PLUSHY:" genome))
         push (pl/plushy->push genome)
         _ (when verbose (println "PUSH:" push))
         ast (::co/ast (:ast (co/push->ast {:push push
                                      :ret-type ret-type
                                      :type-env lib/type-env})))
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
         ast (::co/ast (:ast (co/push->ast
                        (assoc
                         enhanced-task
                         :locals locals
                         :push push
                         :type-env (merge (:type-env enhanced-task)
                                          lib/type-env)))))
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
  [;; Common/Misc
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

   ;; Collections
   'count
   'vec
   'set
   'first
   'last
   'empty?
   'contains?
   'merge
   'disj
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
   'multiply '*
   'quotient `lib/safe-quot
   'divide `lib/safe-div
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
   'isZero 'zero?

   ;; Text
   'charCast `lib/int->char
   'isWhitespace `lib/whitespace?
   'isDigit `lib/digit?
   'isLetter `lib/letter?
   ; `lib/set-char (doesn't exist in clojure)
   'str/capitalize  `str/capitalize
   'str/upper-case  `str/upper-case
   'str/lower-case  `str/lower-case
   'toUpperCase  `lib/char-upper
   'toLowerCase  `lib/char-lower
   
   ;; Collections
   ; `lib/->map --> into {}
   'concat `lib/concat'
   'conj `lib/conj'
   'rest `lib/rest'
   'butlast `lib/butlast'
   'includes? `lib/in?
   'indexOf `lib/index-of
   'index-of `lib/index-of
   'filter `lib/filter'
   'remove `lib/remove'
   ; `lib/remove-element --> type-specific
   'mapcat `lib/mapcat'

   ;; Text/Vec
   'replace `lib/replace'
   'replace-first `lib/replace-first'
   'take `lib/take'
   'reverse `lib/reverse'
   'sort `lib/sort'
   'subs `lib/safe-sub-coll'
   'subvec `lib/safe-sub-coll'

   ;; Vector
   ; `lib/occurrences-of (doesn't exist in clojure?)
   ; 'safe-assoc-nth (typed; assoc on a vec)
   'map-indexed `lib/mapv-indexed
   'distinct `lib/distinctv
   'sort-by `lib/sortv-by
   'group-by 'group-by
   'zipmap 'zipmap

   ;; Set
   'set/union `set/union
   'set/difference `set/difference
   'set/intersection `set/intersection
   'set/subset? `set/subset?
   'set/superset? `set/superset?
   ; `lib/map-set (may delete; keep mapv-set)
   
   ;; Map
   'keys `lib/keys-vec
   ; `lib/keys-set (doesn't exist in clojure?)
   'vals `lib/vals-vec})

(def ast-arity-aliasing
  {'minus {1 `lib/neg
           2 '-
           :default '-}
   'str/join {1 `str/join
              2 'str-join-sep
              :default `str/join}
   'str {1 'str
         2 `lib/concat'
         :default 'str}
   'str/split {1 `lib/split-str-on-ws
               2 `lib/split-str
               :default `lib/split-str}
   'mapv {2 'mapv
          3 `lib/map2v
          :default 'mapv}
   'hash-map {2 '->map1
              4 '->map2
              6 '->map3
              :default '->map1}
   'hash-set {1 '->set1
              2 '->set2
              3 '->set3
              :default '->set1}
   'vector {1 '->vector1
            2 '->vector2
            3 '->vector3
            :default '->vector1}
   'range {1 'range1
           2 'range2
           3 'range3
           :default 'range1}
   'reduce {2 'reduce
            3 'fold
            :default 'reduce}
   'nth {2 `lib/safe-nth
         3 'nth-or-else
         :default `lib/safe-nth}
   'get {2 'get
         3 'get-or-else
         :default 'get}})

(def type-specific-aliasing
  [
   ;'remove ; `lib/remove' `lib/remove-element
   'assoc  ; 'assoc `lib/safe-assoc-nth
  ])

(def special-case-aliasing
  [;; FP
   'comp
   'partial

   ;; Boolean
   ; `lib/and (macro)
   ; `lib/or (macro)
   ])

(def ground-type-alias-map
  {nil `lib/s-var
   "boolean" {:type 'boolean?}
  ;;  'number `lib/INT
  ;;  'number `lib/DOUBLE
   "character" {:type 'char?}
   "string" {:type 'string?}
   "symbol" {:type 'symbol?}
   })

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
  ;; (println "First Task: " task ast-fn-name)
  (cond
    ;; arity-specific functions
    (contains? ast-arity-aliasing ast-fn-name)
    (let [arity-map (get ast-arity-aliasing ast-fn-name)
          fn-symbol (get arity-map
                         (count args)
                         (get arity-map :default))]
      fn-symbol)

    ;; arg-type-specific functions
    ; [!] does NOT work yet
    (contains? type-specific-aliasing ast-fn-name)
    (println "Type-specfic-aliasing not implemented yet!")
    #_(cond
        (if (= 'remove ast-fn-name) ; and first arg is a vector or string 
          "`lib/remove-element"
          "`lib/remove'")
        (if (= 'assoc ast-fn-name) ; and first arg is a vector
          "`lib/safe-assoc-nth"
          "'assoc"))

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
  ([ast task] (decompile-ast ast task {}))
  ([{:keys [op val tag args children] :as ast} task locals]
  ;;  (println "Decomp Task: " task)
   (cond
    ;; Handle constants
     (= :const op)
     (list {:gene :lit
            :val val
            :type (find-type val ast)})

     ;; Handle locals
     (= :local op)
     (do
       (println "locals: " locals)
       (println "name: " (:name ast))

       (list {:gene :local
              :idx (get locals (:name ast))}))

    ;; Handle static method or invoke or var
     (or (= op :static-call)
         (= op :invoke)
         (= op :var))
     (let [ast-fn-name (cond
                         (= op :static-call) ; catch static-call
                         (:method ast)

                         (= op :var) ; catch var (inside invoke)
                         (symbol (-> ast :meta :name))

                         (= (-> ast :fn :op) :var) ; catch var (regular func call)
                         (-> ast :fn :form)

                         :else (-> ast :fn)) ; catch nested invoke 
           raw-decompiled-args (map #(decompile-ast % task locals) args)
           decompiled-args (flatten (reverse raw-decompiled-args))]
       (concat decompiled-args
               (cond
                 (= (ast-fn-name :op) :invoke)
                 (concat (decompile-ast ast-fn-name task locals) (list {:gene :apply}))

                 (= op :var)
                 (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)})

                 :else
                 (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)}
                       {:gene :apply}))))

    ;; Handle quote for lists; translate into vector
     (= op :quote)
     (let [the-vector (vec (-> ast :expr :val))]
       (list {:gene :lit
              :val the-vector
              :type (find-type the-vector (assoc ast :type :vector))}))

    ;; Handle if
     (= op :if)
     (let [ast-fn-name 'if
           raw-decompiled-args (map #(decompile-ast % task locals) (map ast children))
           decompiled-args (flatten (reverse raw-decompiled-args))]
       (concat decompiled-args
               (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)}
                     {:gene :apply})))

    ;; Handle let
     (= op :let)
     (let [[final-locals init-forms]
           (reduce
            (fn [[running-locals forms] {:keys [name init]}]
              (let [idx (count running-locals)
                    updated-locals (assoc running-locals name idx)
                    decompiled-init (decompile-ast init task updated-locals)]
                [updated-locals (conj forms decompiled-init {:gene :let})]))
            [locals []]
            (:bindings ast))
             ;; decompile body using all the updated locals
           decompiled-body (decompile-ast (:body ast) task final-locals)]
       (flatten (concat init-forms decompiled-body (repeat (count (:bindings ast)) {:gene :close}))))
    ;; ;; old code!!
    ;;  (= op :let) 
    ;;    (let [ _ (println "local-name: " (map :name (:bindings ast))) 
    ;;          new-locals (apply merge (map (fn [local-name] 
    ;;                        (if (not (contains? locals local-name))
    ;;                          (assoc locals local-name (count locals)))) (map :name (:bindings ast))))
    ;;          _ (println "new locals: " new-locals) 
    ;;          decompiled-body (decompile-ast (:body ast) task new-locals)]
    ;;      (flatten (list {:gene :let} decompiled-body {:gene :close})))

    ;;  (= op :let)
    ;;    (let [_ (println "local-name: " (map :name (:bindings ast)))
    ;;          [new-locals _]
    ;;          (reduce
    ;;           (fn [[running-locals idx] local-name]
    ;;             (if (contains? running-locals local-name)
    ;;               [running-locals idx]
    ;;               [(assoc running-locals local-name idx) (inc idx)]))
    ;;           [locals (count locals)]
    ;;           (map :name (:bindings ast)))
    ;;          _ (println "new locals: " new-locals)
    ;;          decompiled-body (decompile-ast (:body ast) task new-locals)
    ;;          locals-vals (map #(decompile-ast (:init %) task new-locals) (:bindings ast))]
    ;;      (flatten (list locals-vals {:gene :let} decompiled-body {:gene :close})))

     ;; Handle anonymous function abstraction
     (= op :fn)
     (let [method         (first (:methods ast))
           param-names    (map :name (:params method))
           [updated-locals _]
           (reduce
            (fn [[running-locals idx] param-name]
              (if (contains? running-locals param-name)
                [running-locals idx]
                [(assoc running-locals param-name idx) (inc idx)]))
            [locals 0]
            param-names)
           decompiled-body (decompile-ast (:body method) task updated-locals)
      ;; Resolve the return type or generate a fresh one
           return-type (get ground-type-alias-map
                            (.getName (:return-tag ast))
                            (lib/s-var (gensym "s-")))]
       (list {:gene :fn :arg-types [(lib/s-var (gensym "s-"))] :ret-type return-type} decompiled-body {:gene :close}))
    
    ;; ;; old code!!
    ;; ;; Handle anonymous function abstraction
    ;;  (= op :fn)
    ;;  (let [;; locals (map :form (-> (first (-> ast :methods)) :params)) ; diff way to reach into :methods [] ?
    ;;        decompiled-body (decompile-ast (-> (first (-> ast :methods)) :body) task locals)
    ;;        return-type (get ground-type-alias-map (.getName (-> ast :return-tag)) (lib/s-var (gensym "s-")))]
    ;;    #_(do (println "locals: " locals)
    ;;          (println ":(  : " (conj () (vec decompiled-body) {:gene :fn :arg-types [`lib/s-var] :ret-type return-type}))
    ;;          (println "body: " (vec decompiled-body)))
    ;;    (list {:gene :fn :arg-types [(lib/s-var (gensym "s-"))] :ret-type return-type} decompiled-body {:gene :close}))

     (= op :def)
     (decompile-ast (-> ast
                        :init
                        :expr
                        :methods
                        first
                        :body)
                    task
                    locals)

     :else
     (do
       (println "not handled yet AST op:" op)
       (println "failing AST: \n" ast)
       (println "---------------------------")
       nil))))

(comment

  ;; works 
  (compile-debugging (decompile-ast (ana.jvm/analyze '(remove #(zero? %) [0 2 3 3 0]))) {:type :vector :child {:type 'int?}})
  (decompile-ast (ana.jvm/analyze '(remove #(zero? %) [0 2 3 3 0])))
  (decompile-ast (ana.jvm/analyze '(fn [x] (+ x 1))))
  (decompile-ast (ana.jvm/analyze '(+ ((partial + 2) 3) 10)))

  ;; doesn't work (WIP)
  (decompile-ast (ana.jvm/analyze '(and true false)))

  ;; testing
  (log/set-min-level! :trace) 
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing

(comment
  
  (compile-debugging (decompile-ast (ana.jvm/analyze '(nth [1.0 2.0 3.0 5.0] 3 4.04)))
                     {:type 'double?})
  
;;;; THESE DON'T WORK 

  (compile-debugging (decompile-ast (ana.jvm/analyze '(or true false)))
                     {:type 'boolean?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(< 4 5 8)))
                     {:type 'boolean?}) 
  
  (compile-debugging (decompile-ast (ana.jvm/analyze '((partial + 1) 2))) {:type 'int?} true)

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

  (ana.jvm/analyze 'count)
  )


(pl/plushy->push (decompile-ast (ana.jvm/analyze '(let [x 2
                                                           y 3] (+ x y)))))