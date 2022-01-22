(ns erp12.cbgp-lite.benchmark.psb-utils
  (:require [clojure.set :as set]
            [psb2.core :as psb2]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.gp.pluhsy :as pl]
            [clojure.java.io :as io])
  (:import (java.util.zip GZIPInputStream)))

(defn problem-types
  [{:keys [input->type return-type other-types]}]
  (set/union (set (vals input->type)) #{return-type} other-types))

(defn genetic-source
  [{:keys [input->type literals lit-generators gene-distribution] :as info}]
  (let [types (problem-types info)
        opts {:inputs            (keys input->type)
              :vars              (vec (keys (lib/lib-for-types types)))
              :lits              literals
              :lit-generators    lit-generators
              :abstraction       (vec (concat [:let [:fn]]
                                              ;; 1-arg functions
                                              (map (partial vector :fn) types)
                                              ;; 2-arg functions
                                              (for [arg1 types
                                                    arg2 types]
                                                [:fn arg1 arg2])))
              :gene-distribution gene-distribution}]
    #(pl/random-gene opts)))

(defn reshape-case
  [case]
  {:inputs (-> case
               (dissoc :output1)
               (->> (sort-by first)
                    (map second)))
   :output (:output1 case)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PSB1 Utils

(defn gunzip
  [input output & opts]
  (with-open [input (-> input io/input-stream GZIPInputStream.)]
    (apply io/copy input output opts)))

(defn gunzip-if-not-exists!
  [file]
  (when (not (.exists file))
    (gunzip (io/file (str (.getAbsolutePath file) ".gz"))
            file)))

(defn psb1-read-examples
  [datasets-directory problem-name n-train n-test]
  (let [problem-dir (io/file datasets-directory problem-name)
        ext ".edn"
        edge-file (io/file (.getAbsolutePath problem-dir)
                           (str problem-name "-edge" ext))
        random-file (io/file (.getAbsolutePath problem-dir)
                             (str problem-name "-random" ext))]
    (gunzip-if-not-exists! edge-file)
    (gunzip-if-not-exists! random-file)
    (let [edge (psb2/load-edn-lines edge-file)
          random (psb2/load-edn-lines random-file)]
      {:train (if (< n-train (count edge))
                (psb2/sample edge n-train)
                (concat edge
                        (psb2/sample random
                                     (- n-train (count edge)))))
       :test  (psb2/sample random n-test)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC Generators

(defn rand-bool
  []
  (> (rand) 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loss Functions

(defn absolute-distance
  [actual expected]
  (Math/abs (- actual expected)))
