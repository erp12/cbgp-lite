(ns dev
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def filepath "log.txt")

(def exceed-mem-by-generation
  (with-open [rdr (io/reader filepath)]
    (->> (line-seq rdr)
         (filter #(str/includes? % ":num-exceed-mem-guard"))
         (mapv #(parse-long (second (str/split % #"num-exceed-mem-guard ")))))))

(def total-exceed-by-generation
    (reduce + exceed-mem-by-generation))


(comment

  exceed-mem-by-generation
  total-exceed-by-generation

  (def lib-ns (create-ns 'cbgp.lib))

  (def form '(fn [x y] (abs (- x y))))

  (intern lib-ns 'f-123 (eval form))

  (cbgp.lib/f-123 8 10)


  (comment)
  )