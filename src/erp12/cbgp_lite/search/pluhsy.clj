(ns erp12.cbgp-lite.search.pluhsy
  (:require [erp12.cbgp-lite.utils :as u]))

(defn random-gene
  [{:keys [gene-distribution vars lits lit-generators abstraction]
    :as   opts}]
  (let [kind (u/rand-weighted gene-distribution)
        gene-val (cond
                   (= :apply kind) :apply
                   (= :close kind) :close
                   (= :var kind) (u/safe-rand-nth vars)
                   (= :local kind) (rand-int Integer/MAX_VALUE)
                   (= :lit kind) (u/safe-rand-nth lits)
                   (= :lit-generator kind) (when-let [gen (u/safe-rand-nth lit-generators)] (gen))
                   (= :abstraction kind) (u/safe-rand-nth abstraction)
                   :else (throw (ex-info (str "Unknown kind of gene: " kind)
                                         {:gene-kind         kind
                                          :gene-distribution gene-distribution})))]
    (cond
      ;; Gene value will be nil if there are no possible values (empty coll)
      ;; for a certain gene type. Try again.
      (nil? gene-val)
      (recur opts)

      (contains? #{:var :local} kind)
      [:var gene-val]

      (contains? #{:lit :lit-generator} kind)
      [:lit gene-val]

      :else gene-val)))

(defn random-plushy-genome
  [{:keys [min-genome-size max-genome-size genetic-source]}]
  (repeatedly (+ (rand-int (- max-genome-size min-genome-size)) min-genome-size)
              genetic-source))

(defn plushy->push
  [plushy]
  (loop [plushy plushy
         push []]
    (let [has-open (some #{:open} push)
          gene (first plushy)]
      (if (empty? plushy)
        (if has-open
          ;; If there is no more plushy genes but the push code still contains
          ;; an :open, add :close to the plushy genome and recur.
          (recur '(:close) push)
          ;; Otherwise, return the translated Push code.
          push)
        (cond
          (= gene :close)
          (if has-open
            ;; If there is an :open for this :close gene to close, wrap the Push
            ;; code after the :open with in a list.
            (recur (rest plushy)
                   (let [post-open (->> push
                                        reverse
                                        (take-while (comp not #{:open}))
                                        reverse
                                        vec)
                         open-idx (- (count push) (count post-open) 1)
                         pre-open (take open-idx push)]
                     (vec (concat pre-open [post-open]))))
            ;; If there are no :open in the Push code, this :close gene is a noop.
            (recur (rest plushy)
                   push))

          ;; If the kind of gene implies the opening of a chunk (only `let` and `fn` genes)
          ;; prepend an :open to the plushy genome and append the gene to the Push code
          ;; so that it is followed by nested Push "chunk".
          (or (= gene :let)
              (and (vector? gene)
                   (= (first gene) :fn)
                   (> (count gene) 1)))
          (recur (cons :open (rest plushy))
                 (conj push gene))

          ;; Any other gene is added to the end of the Push code.
          :else
          (recur (rest plushy)
                 (conj push gene)))))))
