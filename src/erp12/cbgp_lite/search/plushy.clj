(ns erp12.cbgp-lite.search.plushy
  (:require [erp12.cbgp-lite.utils :as u]))

(defn prob-by-gene-kind
  [genes kind-distribution]
  (let [kind-distribution (u/weights-to-probs kind-distribution)
        kind-prob (->> genes
                       (map :gene)
                       frequencies
                       (map (fn [[kind freq]]
                              [kind (/ (kind-distribution kind) freq)]))
                       (into {}))]
    (->> genes
         (map (fn [g] [g (kind-prob (:gene g))]))
         (into {}))))

(defn make-genetic-source
  [gene+prob]
  (let [gene+prob (seq gene+prob)
        weights (reductions + (map second gene+prob))
        total (last weights)
        choices (map vector (map first gene+prob) weights)]
    (fn []
      (let [r (rand total)]
        (loop [[[el w] & more] choices]
          (when w
            (if (< r w)
              (let [kind (:gene el)]
                (case kind
                  ;; Convert ERCs into true genes.
                  :lit-generator {:gene :lit :val ((:fn el)) :type (:type el)}
                  :local {:gene :local :idx (rand-int Integer/MAX_VALUE)}
                  el))
              (recur more))))))))


(defn random-plushy-genome
  [{:keys [min-genome-size max-genome-size genetic-source]}]
  (repeatedly (+ (rand-int (- max-genome-size min-genome-size)) min-genome-size)
              genetic-source))

(def ^:private open {:gene :open})
(def ^:private close {:gene :close})

(defn plushy->push
  [plushy]
  (loop [plushy plushy
         push []]
    (let [has-open (some #{open} push)
          gene (first plushy)
          kind (:gene gene)]
      (if (empty? plushy)
        (if has-open
          ;; If there is no more plushy genes but the push code still contains
          ;; an open, add a close to the plushy genome and recur.
          (recur (list close) push)
          ;; Otherwise, return the translated Push code.
          push)
        (cond
          (= kind :close)
          (if has-open
            ;; If there is an :open for this :close gene to close, wrap the Push
            ;; code after the :open with in a list.
            (recur (rest plushy)
                   (let [post-open (->> push
                                        reverse
                                        (take-while (comp not #{open}))
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
          (or (= kind :let)
              (and (= kind :fn)
                   (seq (:arg-types gene))))
          (recur (cons open (rest plushy))
                 (conj push gene))

          ;; Any other gene is added to the end of the Push code.
          :else
          (recur (rest plushy)
                 (conj push gene)))))))
