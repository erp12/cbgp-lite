(ns erp12.cbgp-lite.gp.pluhsy
  (:require [erp12.cbgp-lite.utils :as u]))

(defn random-gene
  [{:keys [gene-distribution inputs vars lits lit-generators abstraction]
    :as   opts}]
  (let [kind (u/rand-weighted gene-distribution)
        gene-val (cond
                   (= :apply kind) :apply
                   (= :open-close kind) (rand-nth [:open :close])
                   (= :input kind) (u/safe-rand-nth inputs)
                   (= :var kind) (u/safe-rand-nth vars)
                   (= :lit kind) (u/safe-rand-nth lits)
                   (= :lit-generator kind) ((u/safe-rand-nth lit-generators))
                   (= :abstraction kind) (u/safe-rand-nth abstraction)
                   :else (throw (ex-info (str "Unknown kind of gene: " kind)
                                         {:gene-kind         kind
                                          :gene-distribution gene-distribution})))]
    (cond
      ;; Gene value will be nil if there are no possible values (empty coll)
      ;; for a certain gene type. Try again.
      (nil? gene-val)
      (recur opts)

      (contains? #{:input :var} kind)
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
        (if (= gene :close)
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
          ;; Any other gene is added to the end of the Push code.
          (recur (rest plushy)
                 (conj push gene)))))))
