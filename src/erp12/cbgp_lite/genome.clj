(ns erp12.cbgp-lite.genome
  "Gene types, genetic sources, and translation from Plushy genomes to Push code.")

;; A gene for creating a variable expression.
;;   - `sym` is the symbol of the variable.
(defrecord Var [sym])

;; A gene for creating a local variable reference.
;;   - `id` a natural number used to resolve the local gene to a Var.
(defrecord Local [idx])

;; A gene for creating a literal expression.
;;   - `val` the value of the literal.
;;   - `typ` the data type of the literal.
(defrecord Lit [val typ])

;; A gene containing a generator `func` for values of `typ`. Ephemeral random constant generator.
;;   - `func` a nullary function for generating a literal value.
;;   - `typ` the data type of the literal.
(defrecord LitGenerator [func typ])

;; A gene for triggering the creation of a function application expression.
(defrecord App [])

;; A gene for triggering the compilation of a lambda function 
;;   - `param-types` a vector of types the function expects as input.
;;   - `ret-type` the type of the returned value.
;;   - After sampling, `param-symbols` is a vector of unique symbols to use as argument names.
;;   - After translation, `push` is a chunk of Push code to compile into the function's body.
(defrecord Abs [param-types ret-type])

;; A gene for triggering the compilation of a let binding.
;; - After sampling, `sym` is a unique symbol to use as the local variable.
;; - After translation, `push` is a chunk of Push code to compile into the body of the let expression.
(defrecord Let [])

(defn gene-type
  "The type of gene represented as a keyword."
  [gene]
  (cond
    (contains? #{:open :close} gene) gene
    (instance? erp12.cbgp_lite.genome.Var gene) :var
    (instance? erp12.cbgp_lite.genome.Local gene) :local
    (instance? erp12.cbgp_lite.genome.Lit gene) :lit
    (instance? erp12.cbgp_lite.genome.LitGenerator gene) :lit-gen
    (instance? erp12.cbgp_lite.genome.App gene) :app
    (instance? erp12.cbgp_lite.genome.Abs gene) :abs
    (instance? erp12.cbgp_lite.genome.Let gene) :let
    :else (throw (ex-info "Unknown gene type" {:gene gene
                                               :class (class gene)}))))

(def gene-type-distribution
  ;; @todo parameterize?
  ;; @todo These defaults should be hyperparamter tuned. Maybe use real code?

  ;; The values should sum to 1.0.
  ;; Rules of thumb:
  ;;   app = var + abs
  ;;   close = abs + let
  {:var 0.2
   :local 0.2
   :lit 0.2
   :lit-gen 0.05
   :app 0.2
   :abs 0.05
   :let 0.05
   :close 0.05})

(defn make-genetic-source
  "Creates a genetic source. A genetic source is a nullary function that 
   returns a randomly sampled gene from `genes` according to the gene-type-distribution."
  [genes]
  (let [genes-by-type     (group-by gene-type genes)
        ;; Get the gene types found in the collection of genes.
        gene-types        (vec (keys genes-by-type))
        ;; Grab the probabilities for the gene types found in `genes`.
        ;; Essentially, this filters the distribution to just the gene types that were actually found.
        gene-type-weights (mapv gene-type-distribution gene-types)
        ;; Cumulative distribution function for gene types.
        cdf               (reductions + gene-type-weights)
        ;; The total of the distribution.
        ;; Typically will be 1.0, but will be lower when not all gene types are present.
        total             (last cdf)
        gene-type-cdf     (mapv vector gene-types cdf)]
    (fn genetic-source []
      (let [r         (rand total)
            ;; Sample a gene-type using CDF weights.
            gene-type (loop [[[el p] & more] gene-type-cdf]
                        (when p
                          (if (< r p)
                            el
                            (recur more))))
            ;; Pick a gene of the correct type with uniform probability.
            gene      (rand-nth (genes-by-type gene-type))]
        (case gene-type
          :local (->Local (rand-int Integer/MAX_VALUE))
          :lit-gen (->Lit ((:func gene)) (:typ gene))
          ;; Assign random unique parameter names to each Abs gene.
          :abs (assoc gene
                      :param-symbols (repeatedly (count (:param-types gene)) #(gensym "p-")))
          ;; Assign a random unique local variable name to each Let gene.
          :let (assoc gene
                      :sym (gensym "l-"))
          gene)))))

;; Helpers for creating genes manually.

(defn var-gene [{:keys [sym]}] (->Var sym))
(defn local-gene [{:keys [idx]}] (->Local idx))
(defn lit-gene [{:keys [val typ]}] (->Lit val typ))
(defn lit-gen-gene [{:keys [func typ]}] (->LitGenerator func typ))
(defn app-gene [] (->App))
(defn abs-gene [{:keys [param-types ret-type]}] (->Abs param-types ret-type))
(defn let-gene [] (->Let))

(defn random-plushy-genome
  "Creates a plush genome of random length within the given range
  containing genes sampled from the given genetic source."
  [{:keys [min-genome-size max-genome-size genetic-source]}]
  (repeatedly (+ (rand-int (- max-genome-size min-genome-size)) min-genome-size)
              genetic-source))

(defn plushy->push
  "Translates the plushy genome into push code."
  [plushy]
  (loop [[gene & more-plushy] plushy
         push   []]
    (let [has-open (some #{:open} push)
          g-typ    (when gene (gene-type gene))]
      (if gene
        ;; Translate next gene
        (cond
          (= g-typ :close)
          (if has-open
            ;; If there is an :open for this :close gene to close, wrap the Push
            ;; code after the :open with in a list.
            (recur more-plushy
                   (let [post-open (->> push
                                        (reverse)
                                        (take-while (comp not #{:open}))
                                        (reverse)
                                        (vec))
                         open-idx  (- (count push) (count post-open) 1)
                         pre-open  (take open-idx push)]
                     ;; Insert the push chunk into the object (Fn or Let) directly before the open.
                     (conj (vec (butlast pre-open))
                           (assoc (last pre-open) :push post-open))))
            ;; If there are no :open in the Push code, this :close gene is a noop.
            (recur more-plushy
                   push))

          ;; If the kind of gene implies the opening of a chunk (only `let` and `fn` genes)
          ;; prepend an :open to the plushy genome and append the gene to the Push code
          ;; so that it is followed by nested Push "chunk".
          (or (= g-typ :let)
              (and (= g-typ :abs)
                   (seq (:param-types gene))))
          (recur (cons :open more-plushy)
                 (conj push gene))

          ;; Any other gene is added to the end of the Push code.
          :else
          (recur more-plushy
                 (conj push gene)))
        ;; No more genes to translate.
        (if has-open
          ;; If there is no more plushy genes but the push code still contains
          ;; an open, add a close to the plushy genome and recur.
          (recur [:close] push)
          ;; Otherwise, return the translated Push code.
          push)))))
