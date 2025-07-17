(ns erp12.cbgp-lite.lang.decompile
  (:require [clojure.tools.analyzer.jvm :as ana.jvm]
            [erp12.cbgp-lite.lang.ast :as ast]
            [erp12.cbgp-lite.lang.compile :as co]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.search.plushy :as pl]
            [erp12.cbgp-lite.task :as tsk]
            [taoensso.timbre :as log]
            [clojure.set :as set]))

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
  [;; FP
   'comp
   'partial
   
   ;; Common/Misc
   'not
   'not=
   'if
   'print
   'println

   ;; Boolean
   `lib/and
   `lib/or

   ;; Numeric 
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
   'mod `lib/safe-mod
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
   ; `lib/->map --> in Clojure: into {}
   'concat `lib/concat'
   'conj `lib/conj'
   'rest `lib/rest'
   'butlast `lib/butlast'
   'includes? `lib/in?
   'indexOf `lib/index-of
   'index-of `lib/index-of
   'filter `lib/filter'
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
         :default 'get}
   'do {2 'do2
        3 'do3
        :default 'do2}})

(def type-specific-aliasing
  #{'remove
    'assoc})

(def special-case-aliasing
  [])
   ;; Boolean
   ; `lib/and (macro)
   ; `lib/or (macro)
   

(def ground-type-alias-map
  {nil {:type :s-var :sym 'a}
   "boolean" {:type 'boolean?}
  ;;  'number `lib/INT
  ;;  'number `lib/DOUBLE
   "character" {:type 'char?}
   "string" {:type 'string?}
   "symbol" {:type 'symbol?}})
   

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
  (cond
    ;; arity-specific functions
    (contains? ast-arity-aliasing ast-fn-name)
    (let [arity-map (get ast-arity-aliasing ast-fn-name)
          fn-symbol (get arity-map
                         (count args)
                         (get arity-map :default))]
      fn-symbol)

    ;; arg-type-specific functions
    (contains? type-specific-aliasing ast-fn-name)
    (cond
      (= 'remove ast-fn-name)
      (if (= :set (:type (first args))) 
        `lib/remove-element
        `lib/remove')
      
      (= 'assoc ast-fn-name)
      (if (= :vector (:type (first args)))
        `lib/safe-assoc-nth
        'assoc))

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

;; (defn push-scope! [locals]
;;   (swap! locals conj {}))

;; (defn pop-scope! [locals]
;;   (swap! locals pop))

;; (defn add-to-locals-scope! [locals param-names]
;;   (let [current-scope (peek @locals)
;;         start-idx (count current-scope)
;;         new-bindings (into {}
;;                            (map-indexed (fn [i name]
;;                                           [name (+ start-idx i)])
;;                                         param-names))
;;         new-scope (merge current-scope new-bindings)]
;;     (swap! locals #(conj (pop %) new-scope))))

;; (defn resolve-local [locals name]
;;   (some (fn [scope] (get scope name))
;;         (reverse @locals)))

;; (defn decompile-ast*
;;   ([ast] (decompile-ast* ast {}))
;;   ([ast task] (decompile-ast* ast task (atom [{}])))
;;   ([{:keys [op val tag args children] :as ast} task locals]
;;    (cond
;;      ;; constants
;;      (= :const op)
;;      (list {:gene :lit
;;             :val val
;;             :type (find-type val ast)})

;;      ;; locals
;;      (= :local op)
;;      (let [idx (resolve-local locals (:name ast))]
;;        (when (nil? idx)
;;          (throw (ex-info (str "Unbound local: " (:name ast)) {:locals @locals})))
;;        (list {:gene :local :idx idx}))

;;      ;; vars, invokes, static calls
;;      (or (= op :static-call) (= op :invoke) (= op :var))
;;      (let [ast-fn-name (cond
;;                          (= op :static-call) (:method ast)
;;                          (= op :var) (symbol (-> ast :meta :name))
;;                          (= (-> ast :fn :op) :var) (-> ast :fn :form)
;;                          :else (-> ast :fn))
;;            raw-decompiled-args (map #(decompile-ast* % task locals) args)
;;            decompiled-args (flatten (reverse raw-decompiled-args))]
;;        (concat decompiled-args
;;                (cond
;;                  (= (ast-fn-name :op) :invoke)
;;                  (concat (decompile-ast* ast-fn-name task locals) (list {:gene :apply}))
;;                  (= (ast-fn-name :op) :local)
;;                  (concat (decompile-ast* ast-fn-name task locals) (list {:gene :apply}))
;;                  (= (ast-fn-name :op) :fn)
;;                  (concat (decompile-ast* ast-fn-name task locals) (list {:gene :apply}))
;;                  (= op :var)
;;                  (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)})
;;                  :else
;;                  (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)}
;;                        {:gene :apply}))))

;;      ;; quoted list
;;      (= op :quote)
;;      (let [the-vector (vec (-> ast :expr :val))]
;;        (list {:gene :lit
;;               :val the-vector
;;               :type (find-type the-vector (assoc ast :type :vector))}))

;;      ;; if form
;;      (= op :if)
;;      (let [ast-fn-name 'if
;;            raw-decompiled-args (map #(decompile-ast* % task locals) (map ast children))
;;            decompiled-args (flatten (reverse raw-decompiled-args))]
;;        (concat decompiled-args
;;                (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)}
;;                      {:gene :apply})))

;;      ;; let
;;      (= op :let)
;;      (do
;;        (push-scope! locals)
;;        (let [[_ init-forms]
;;              (reduce
;;               (fn [[_ forms] {:keys [name init]}]
;;                 (add-to-locals-scope! locals [name])
;;                 [@locals (conj forms (decompile-ast* init task locals) {:gene :let})])
;;               [locals []]
;;               (:bindings ast))
;;              decompiled-body (decompile-ast* (:body ast) task locals)]
;;          (pop-scope! locals)
;;          (flatten (concat init-forms decompiled-body
;;                           (repeat (count (:bindings ast)) {:gene :close})))))

;;      ;; fn (anonymous)
;;      (= op :fn)
;;      (do
;;        (push-scope! locals)
;;        (let [params (:params (first (:methods ast)))
;;              param-names (map :name params)
;;              arg-count (count param-names)
;;              arg-types (vec (repeatedly arg-count #(lib/s-var (gensym "s-"))))
;;              _ (add-to-locals-scope! locals param-names)
;;              decompiled-body (decompile-ast* (:body (first (:methods ast))) task locals)
;;              return-type (if-let [tag (:return-tag ast)]
;;                            (get ground-type-alias-map (.getName tag) (lib/s-var (gensym "s-")))
;;                            (lib/s-var (gensym "s-")))]
;;          (pop-scope! locals)
;;          (list {:gene :fn :arg-types arg-types :ret-type return-type}
;;                decompiled-body
;;                {:gene :close})))

;;      ;; def
;;      (= op :def)
;;      (decompile-ast* (-> ast :init :expr :methods first :body)
;;                      task
;;                      locals)

;;      ;; fallback
;;      :else
;;      (do
;;        (println "not handled yet AST op:" op)
;;        (println "failing AST: \n" ast)
;;        (println "---------------------------")
;;        nil))))

;; (defn decompile-ast
;;   ([ast]
;;    (decompile-ast* ast {} (atom [{}])))
;;   ([ast task]
;;    (decompile-ast* ast task (atom [{}]))))



(defn add-to-locals-map
  "Add all new locals in a given list to the decompile-ast locals map"
  ([locals locals-to-add] (add-to-locals-map locals locals-to-add 0))
  ([locals locals-to-add parity-offset] 
   (doseq [local-name locals-to-add]
     (if (nil? (get @locals local-name))
       (swap! locals assoc local-name (- (count @locals) (+ 1 parity-offset))))))) 
   
(defn remove-from-locals-map
  "Remove all locals in a given list from the decompile-ast locals map"
  [locals locals-to-rm] 
  (doseq [local-name locals-to-rm]
      (swap! locals dissoc local-name)))

(defn decompile-ast*
  "Decompiles AST into a CBGP genome."
  ([ast] (decompile-ast* ast {}))
  ([ast task] (decompile-ast* ast task {}))
  ([{:keys [op val tag args children] :as ast} task locals]
  ;;  (println "--------------------------")
  ;;  (println "[DECOMPILE INFO]")
  ;; ;;  (println "-> AST: " ast)
  ;; ;;  (println "-> TASK: " task)
  ;;  (println "-> LOCALS: " @locals)
  ;;  (println "-> OFFSET? " (get @locals :locals-parity-offset))
  ;;  (println "-> CURR OP: " op "\n")
   (cond
    ;; Handle constants
     (= :const op)
     (list {:gene :lit
            :val val
            :type (find-type val ast)})

     ;; Handle locals
     (= :local op)
     (let [parity-offset (if (get @locals :locals-parity-offset) 1 0)
           _ (if (nil? (get @locals (:name ast)))
               (swap! locals assoc (:name ast) (- (count @locals) (+ 1 parity-offset))))
           local_val (get @locals (:name ast) 0)]
       (list {:gene :local :idx local_val}))

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
                         :else
                         (-> ast :fn)) ; catch nested invoke 
           raw-decompiled-args (map #(decompile-ast* % task locals) args)
           decompiled-args (flatten (reverse raw-decompiled-args))]
           
       (concat decompiled-args
               (cond
                 (= (ast-fn-name :op) :invoke)
                     (concat (decompile-ast* ast-fn-name task locals) (list {:gene :apply}))
                 (= (ast-fn-name :op) :local)
                 (concat (decompile-ast* ast-fn-name task locals) (list {:gene :apply}))
                 (= (ast-fn-name :op) :fn)
                 (concat (decompile-ast* ast-fn-name task locals) (list {:gene :apply}))
                 (= op :var)
                 (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)})
                 :else (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)}
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
           raw-decompiled-args (map #(decompile-ast* % task locals) (map ast children))
           decompiled-args (flatten (reverse raw-decompiled-args))]
       (concat decompiled-args
               (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)}
                     {:gene :apply})))

;; Handle let
     (= op :let)
     (let [_ (swap! locals update :locals-parity-offset not)
           [final-locals init-forms]
           (reduce
            (fn [[running-locals forms] {:keys [name init]}]
              (let [_ (add-to-locals-map locals [name])
                    decompiled-init (decompile-ast* init task locals)]
                [@locals (conj forms decompiled-init {:gene :let})]))
            [locals []]
            (:bindings ast))

          ;; decompile body using all the updated locals
           decompiled-body (decompile-ast* (:body ast) task locals)]
       (flatten (concat init-forms decompiled-body (repeat (count (:bindings ast)) {:gene :close}))))

;; Handle anonymous function abstraction
     (= op :fn)
     (let [param-names (map :name (:params (first (:methods ast)))) 
           parity-offset (if (get @locals :locals-parity-offset) 1 0)
           _ (add-to-locals-map locals param-names parity-offset)

           decompiled-body (decompile-ast* (:body (first (:methods ast))) task locals)
           arg-count (count (-> ast :methods first :params)) ; [!] any case w/ multiple :methods?
           arg-types (vec (repeatedly arg-count #(lib/s-var (gensym "s-"))))
          ;;  _ (println "FN - finished arg decomp...")
      ;; Resolve the return type or generate a fresh one
           return-type (if (nil? (:return-tag ast))
                         (lib/s-var (gensym "s-"))
                         (get ground-type-alias-map (.getName (:return-tag ast)) (lib/s-var (gensym "s-"))))]
       (remove-from-locals-map locals param-names)
       (flatten (list {:gene :fn :arg-types arg-types :ret-type return-type} decompiled-body {:gene :close})))
     
     (= op :def)
     (decompile-ast* (-> ast
                         :init
                         :expr
                         :methods
                         first
                         :body)
                     task
                     locals)

     ; handle do -- not working :Y
     (= op :do)
     (do (println "DO - not tested AST op")
         (flatten (list (map #(decompile-ast* % task locals) (:statements ast))
                        {:gene :var :name (get-fn-symbol 'do tag (:statements ast) task)}
                        {:gene :apply})))

     :else
     (do
       (println "not handled yet AST op:" op)
      ;;  (println "failing AST: \n" ast)
       (println "---------------------------")
       nil))))

(defn decompile-ast
  "Decompiles AST into a CBGP genome."
  ([ast] (decompile-ast ast {}))
  ([ast task]
   (let [locals-map (atom {:locals-parity-offset false})]
     (decompile-ast* ast task locals-map)))
  ; [!] TO DO: make this work w/ verbose
  #_([ast task verbose]
     (let [locals-map (atom {})]
       (decompile-ast ast locals-map verbose))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing
(comment
;;;; THESE DON'T WORK 
  (compile-debugging (decompile-ast (ana.jvm/analyze '(< 4 5 8)))
                     {:type 'boolean?}) ; no multi-arity for comparison funcs
  
  (compile-debugging (decompile-ast (ana.jvm/analyze '(or true false)))
                     {:type 'boolean?})
  (compile-debugging (decompile-ast (ana.jvm/analyze '(and 0 1))) {:type 'boolean?})

  ; and AST composition:
  ; :op :let
  ; :children [:bindings :body]
  ; :bindings [{ <children/locals> }]
  ; :body {:op :if :children [:test :then :else]}
  ; --> :test {:name and... :op :local} *binds 'and' to 'true' cond? (first truthy value)
  ; --> :then {:op :const :val false}
  ; --> :else {:name and... :op :local} *does same as else?
  
  ; accessing "and"
  (decompile-ast (ana.jvm/analyze '(and true true)))
  (clojure.string/includes?
   (-> (ana.jvm/analyze '(or true true)) :bindings first :name str)
   "or")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; LET/FN TESTING
  (log/set-min-level! :trace) 

  ;; testing llm code
  ; niche defn issue:
  ; analyzing a called defn breaks b/c no condition in handle methods for :op :invoke -> :op :def
  (decompile-ast (ana.jvm/analyze '((defn combine-fns [a b c]
                                        (let [x 3]
                                          (+ x b c))) -1 10 9)))
  (-> (ana.jvm/analyze '((defn combine-fns [a b c]
                           (let [x 3]
                             (+ x b c))) -1 10 9)) 
      :fn
      keys
      )
  
  (compile-debugging (decompile-ast (ana.jvm/analyze '(let [x [3 0 2 0 1 0]
                                                            y (fn [z] (mapv inc ((fn [z3] (conj z3 4)) ((fn [z2] (remove zero? z2)) z))))]
                                                        (y x))))
                     {:type :vector :child {:type 'int?}} true)
  

;; This only works when forcing left and right instead of first and second
  (compile-debugging2
   '({:gene :local, :idx 1}
    {:gene :var, :name right}
    {:gene :apply}
    {:gene :local, :idx 0}
    {:gene :var, :name right}
    {:gene :apply}
    {:gene :var, :name -}
    {:gene :apply}
    {:gene :local, :idx 1}
    {:gene :var, :name left}
    {:gene :apply}
    {:gene :local, :idx 0}
    {:gene :var, :name left}
    {:gene :apply}
    {:gene :var, :name -}
    {:gene :apply}
    {:gene :var, :name *}
    {:gene :apply})
   {:input->type {'input1 {:type :tuple, :children [{:type 'double?} {:type 'double?}]}
                  'input2 {:type :tuple, :children [{:type 'double?} {:type 'double?}]}}
    :ret-type {:type 'double?}} [[0.0 0.0] [1.0 1.0]] true)
  

  )

  