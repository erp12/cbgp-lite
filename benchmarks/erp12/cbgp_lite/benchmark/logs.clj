(ns erp12.cbgp-lite.benchmark.logs
  (:require [clojure.instant :as inst]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io File)))

(defn log-line-ts
  [line]
  ;(println line)
  (try
    (inst/read-instant-date (first (str/split line #" ")))
    (catch RuntimeException _
      nil)))

(defn log-meta
  [^File f]
  (let [parts (-> f .getAbsolutePath (str/replace #"\.txt" "") (str/split #"/") (->> (take-last 6)))]
    (let [[suite problem search _ _ run-id] parts]
      {:suite      suite
       :problem    problem
       :search     search
       :id         (-> run-id (str/replace #"run" "") Integer/parseInt)
       :started    (with-open [rdr (io/reader f)]
                     (log-line-ts (first (line-seq rdr))))
       :last-lines (with-open [rdr (io/reader f)]
                     (vec (take-last 10 (line-seq rdr))))
       :file       f})
    ))

(defn all-logs
  [root]
  (->> root io/file file-seq
       (filter #(str/ends-with? (.getAbsolutePath %) ".txt"))
       (remove #(zero? (.length %)))
       (map log-meta)))

(defn finished?
  [log]
  (some #(or (str/includes? % "SOLUTION")
             (str/includes? % "POST-SIMPLIFICATION")
             ;; When a final solution is only 1 symbol, the logging code produced this error
             ;; No time to re-run experiments.
             (= % "Don't know how to create ISeq from: clojure.lang.Symbol"))
        (:last-lines log)))

(defn ended
  [log]
  (when (finished? log)
    (log-line-ts (last (:last-lines log)))))

(defn duration-millis
  [log]
  (let [end (ended log)]
    (when end
      (- (.getTime end) (.getTime (:started log))))))

(defn solved?
  [log]
  (and (finished? log)
       (some #(str/includes? % "SOLUTION FOUND") (:last-lines log))))

(defn generalized?
  [log]
  (and (solved? log)
       (some #(str/includes? % "SOLUTION GENERALIZED") (:last-lines log))))

(defn code
  [log-line]
  (-> log-line
      (str/split #":(best-)?code")
      second
      (str/split #",")
      first
      read-string))

(defn best-code
  [log]
  (->> log
       :last-lines
       (filter #(str/includes? % "REPORT"))
       last
       code))

;; @todo (defn pre-simplification-code [log] ...)

(defn post-simplification-code
  [log]
  (->> log
       :last-lines
       (filter #(str/includes? % "POST-SIMPLIFICATION"))
       first
       code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entrypoints

(defn finished-run-counts
  [{:keys [root]}]
  (->> root
       name
       all-logs
       (filter finished?)
       (group-by #(select-keys % [:problem :search]))
       (map (fn [[k v]] [k (count v)]))
       (sort-by (comp vec flatten vec first))))

(defn error-runs
  [{:keys [root]}]
  (->> root name all-logs
       (filter (fn [{:keys [last-lines]}]
                 (some #(str/includes? % "Execution error") last-lines)))
       (map #(select-keys % [:search :problem :id]))
       (sort-by #(vector (:search %) (:problem %) (:id %)))))

;; @todo avg-minutes

(defn solution-counts
  [{:keys [root]}]
  (->> root name all-logs
       (filter solved?)
       (group-by #(select-keys % [:problem :search]))
       (map (fn [[k v]] [k (count v)]))
       (sort-by (comp vec flatten vec first))))

(defn solution-rates
  [{:keys [root]}]
  (let [finished (->> root name all-logs (filter finished?))
        grouper #(select-keys % [:problem :search])
        solution-counts (->> finished
                             (filter solved?)
                             (group-by grouper)
                             (map (fn [[k v]] [k (count v)]))
                             (into {}))]
    (->> finished
         (group-by grouper)
         (map (fn [[k v]]
                [k (float (/ (get solution-counts k 0) (count v)))]))
         (sort-by (comp vec flatten vec first)))))

(defn generalized-solution-counts
  [{:keys [root]}]
  (->> root name all-logs
       (filter generalized?)
       (group-by #(select-keys % [:problem :search]))
       (map (fn [[k v]] [k (count v)]))
       (sort-by (comp vec flatten vec first))))

(defn generalized-solution-rates
  [{:keys [root]}]
  (let [finished (->> root name all-logs (filter finished?))
        grouper #(select-keys % [:problem :search])
        solution-counts (->> finished
                             (filter generalized?)
                             (group-by grouper)
                             (map (fn [[k v]] [k (count v)]))
                             (into {}))]
    (->> finished
         (group-by grouper)
         (map (fn [[k v]]
                [k (float (/ (get solution-counts k 0) (count v)))]))
         (sort-by (comp vec flatten vec first)))))

(defn generalized-solutions
  [{:keys [root]}]
  (doseq [{:keys [problem search solutions]}
          (->> root name all-logs
               (filter generalized?)
               (map #(assoc (select-keys % [:problem :search])
                       :code (-> % post-simplification-code pr-str
                                 (str/replace "erp12.cbgp-lite.lang.lib/" "lib/")
                                 (str/replace "clojure.core/" ""))))
               (group-by #(select-keys % [:problem :search]))
               (map (fn [[k v]]
                      (assoc k :solutions (frequencies (map :code v)))))
               (sort-by #(vector (:search %) (:problem %))))]
    (println search problem)
    (doseq [[code freq] (sort-by second < solutions)]
      (println freq "\t" code))))

(defn non-solution-runs
  [{:keys [root]}]
  (->> root name all-logs
       (filter finished?)
       (remove solved?)
       (map #(select-keys % [:problem :search :id]))
       (group-by #(select-keys % [:problem :search]))
       (sort-by (comp vec flatten vec first))))

(defn non-generalizing-solutions
  [{:keys [root]}]
  (->> root name all-logs
       (filter solved?)
       (remove generalized?)
       (map #(select-keys % [:problem :search :id]))
       (group-by #(select-keys % [:problem :search]))
       (sort-by (comp vec flatten vec first))))

(comment

  (def ROOT "data/logs/")

  (finished-run-counts {:root ROOT})
  (solution-counts {:root ROOT})
  (solution-rates {:root ROOT})
  (generalized-solution-counts {:root ROOT})
  (generalized-solution-rates {:root ROOT})
  (generalized-solutions {:root ROOT})

  (error-runs {:root ROOT})
  (non-solution-runs {:root ROOT})
  (non-generalizing-solutions {:root ROOT})

  (->> ROOT all-logs
       (filter #(and (= "simulated-annealing" (:search %))
                     (= "vectors-summed" (:problem %))
                     (generalized? %)))
       first
       best-code)

  (->> ROOT all-logs
       (filter (fn [{:keys [last-lines]}] (some #(str/includes? % "NullPointerException") last-lines)))
       (map #(select-keys % [:search :problem :id]))
       (sort-by #(vector (:search %) (:problem %) (:id %))))

  )