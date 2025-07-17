(ns erp12.cbgp-lite.benchmark.llm-util
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [erp12.cbgp-lite.benchmark.suite.composite :as c]
            [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.decompile :as decompile]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.llm.problem-map :as probmap]
            [psb2.core :as psb2]
            [pyjama.core :as ollama]))  
 
(defn make-program-prompt-model
  [prompt model]
  (with-out-str
    (ollama/ollama
     "http://localhost:11434"
     :generate
     {:prompt prompt
      :model model})))

(defn extract-triple-backtick-code
  [s]
  (let [pattern #"(?s)```(?:\w+\n)?(.*?)```"
        matches (map second (re-seq pattern s))]
    (if (seq matches)
      (first matches)
      s)))

(defn namespace-qualify-macros
  [s]
  (-> s
      (clojure.string/replace #"and " "erp12.cbgp-lite.lang.lib/and ")
      (clojure.string/replace #"or " "erp12.cbgp-lite.lang.lib/or ")))

(defn get-inputs
  [examples]
  (map #(butlast (vals %)) examples))

(def case-generators
  {"sum-2-vals" (fn sum-2-vals-gen []
                  (c/sum-2-vals-case-generator (bu/string-generator 10)))
   "sum-2-vals-polymorphic" (let [key-generators [(bu/string-generator 10)
                                                  (bu/int-generator 1000)
                                                  bu/rand-char
                                                  rand
                                                                          ;; vector of booleans
                                                  #(vec (repeatedly (inc (rand-int 16)) bu/rand-bool))
                                                                          ;; tuple containing a char and an integer
                                                  #(vector (bu/rand-char) (c/rand-int-range -10 10))]]
                              (fn sum-2-vals-polymorphic-gen []
                                (c/sum-2-vals-case-generator (rand-nth key-generators))))
   "sum-2D" (fn sum-2D-gen []
              (let [rows (inc (rand-int 10))
                    cols (inc (rand-int 10))
                    input-matrix (vec (for [_ (range rows)]
                                        (vec (repeatedly cols (bu/int-generator 1000)))))
                    output (reduce + (map #(reduce + %) input-matrix))]
                {:inputs [input-matrix]
                 :output output}))
   "centimeters-to-meters" (fn centimeters-to-meters-gen []
                             (let [in-cm (rand-int 10000)
                                   out-m (quot in-cm 100)
                                   out-cm (mod in-cm 100)]
                               {:inputs [in-cm]
                                :output (vector out-m out-cm)}))
   "set-symmetric-difference" (fn set-symmetric-difference-gen
                                []
                                (let [set-generator (fn [] (set (repeatedly (rand-int 50) #(rand-int 50))))
                                      set1 (set-generator)
                                      set2 (set-generator)
                                      output (set/union (set/difference set1 set2)
                                                        (set/difference set2 set1))]
                                  {:inputs [set1 set2]
                                   :output output}))
   "max-applied-fn" (fn max-applied-fn-case-gen
                      []
                      (let [bound (c/rand-int-range 1 49)
                            the-fn (c/make-int-to-int-fn bound)
                            output (apply max-key the-fn (range bound))]
                        {:inputs [bound the-fn]
                         :output output}))
   "count-true" (fn count-true-gen []
                  (let [{:keys [val-gen preds]} (rand-nth c/value-generators-and-predicates)
                        vector (c/rand-vector 0 50 val-gen)
                        pred (rand-nth preds)]
                    {:inputs [vector pred]
                     :output (count (filter pred vector))}))
   "first-index-of-true" (fn first-index-of-true-gen []
                           (loop [attempt 0]
                             (let [{:keys [val-gen preds]} (rand-nth c/value-generators-and-predicates)
                                   the-vector (c/rand-vector 0 50 val-gen)
                                   pred (rand-nth preds)
                                   output (->> the-vector
                                               (map-indexed vector)
                                               (filter #(pred (second %)))
                                               ffirst)]
                               (if (or (nil? output)
                                       (< output (- 10 attempt)))
                                 (recur (inc attempt))
                                 {:inputs [the-vector pred]
                                  :output output}))))
   "set-cartesian-product" (let [set-generator (fn [] (set (c/rand-vector 0 21 #(rand-int 100))))]
                             (fn cartesian-product-gen
                               []
                               (let [set1 (set-generator)
                                     set2 (set-generator)
                                     output (set (for [x set1
                                                       y set2]
                                                   (vector x y)))]
                                 {:inputs [set1 set2]
                                  :output output})))
   "filter-bounds" (let [generators [(bu/string-generator 10)
                                     bu/rand-char
                                     (bu/int-generator 1000)
                                     (bu/int-generator 20)
                                     rand]]
                     (fn filter-bounds-gen []
                       (let [val-gen (rand-nth generators)
                             the-set (set (c/rand-vector 20 50 val-gen))
                             x (val-gen)
                             y (val-gen)
                             lower (lib/min' x y)
                             upper (lib/max' x y)]
                         {:inputs [the-set lower upper]
                          :output (set (filter #(and (lib/<' lower %) (lib/<' % upper))
                                               the-set))})))

   "area-of-rectangle" (fn area-of-rectangle-gen
                         []
                         (let [xs [(c/rand-float-range -100 100) (c/rand-float-range -100 100)]
                               ys [(c/rand-float-range -100 100) (c/rand-float-range -100 100)]
                               x1 (reduce max xs)
                               x2 (reduce min xs)
                               y1 (reduce max ys)
                               y2 (reduce min ys)
                               output (* (- x1 x2)
                                         (- y1 y2))]
                           {:inputs [[x1 y1] [x2 y2]]
                            :output output}))

   "sum-vector-vals" (fn sum-vector-vals-gen []
                       (let [the-map (first (:inputs (c/sum-2-vals-case-generator (bu/string-generator 10))))
                             prob (+ 0.1 (rand 0.8))
                             the-vector (vec (random-sample prob (keys the-map)))]
                         {:inputs [the-map the-vector]
                          :output (apply + (map the-map the-vector))}))

   "sets-with-element" (fn sets-with-element-gen []
                         (let [max-int 100
                               num-sets (rand-int 25)
                               int-gen #(rand-int max-int)
                               the-int (int-gen)
                               prob (rand) ; prob of including the-int
                               set-gen #(let [s (set (repeatedly (rand-int 25) int-gen))]
                                          (if (< (rand) prob)
                                            (conj s the-int)
                                            (disj s the-int)))
                               the-sets (set (repeatedly num-sets set-gen))
                               output (set (filter #(contains? % the-int)
                                                   the-sets))]
                           {:inputs [the-sets the-int]
                            :output output}))
   "time-sheet" (fn time-sheet-gen []
                  (let [num-records (inc (rand-int 50))
                        num-names (inc (rand-int 10))
                        names (vec (take num-names (shuffle c/names-100)))
                        records (vec (repeatedly num-records #(vector (rand-nth names)
                                                                      (rand-int 50))))
                        the-name (rand-nth names)
                        output (apply + (map second (filter #(= the-name (first %))
                                                            records)))]
                    {:inputs [records the-name]
                     :output output}))
   "min-key" (let [generators [(bu/string-generator 10)
                               bu/rand-char
                               (bu/int-generator 1000)
                               rand
                               bu/rand-bool
                                                       ;; vector of booleans
                               #(vec (repeatedly (inc (rand-int 16)) bu/rand-bool))
                                                       ;; tuple containing a char and an integer
                               #(vector (bu/rand-char) (c/rand-int-range -10 10))]]
               (fn min-key-gen []
                 (let [val-gen (rand-nth generators)
                       the-map (zipmap (c/rand-vector 1 50 val-gen)
                                       (repeatedly (bu/int-generator 1000)))
                       output (first (apply min-key second the-map))]
                                           ;; Ensure the min is unique, i.e. there aren't two keys with same min
                                           ;; Just recur to try again if not.
                   (if (< 1 (count (filter #(= (get the-map output) %)
                                           (vals the-map))))
                     (recur)
                     {:inputs [the-map]
                      :output output}))))
   "simple-encryption" (fn simple-encryption-gen []
                         (let [available-chars (vec (concat [\newline \tab] (map char (range 32 127))))
                                                      ;; These three need to be let here, so that they can be used inside
                                                      ;; of functions without those functions being random when run
                               char-map (zipmap available-chars (shuffle available-chars))
                               offset (c/rand-int-range -20 20)
                               char-map-with-limited-values (zipmap available-chars
                                                                    (let [opts (take (c/rand-int-range 2 6)
                                                                                     (shuffle available-chars))]
                                                                      (repeatedly #(rand-nth opts))))

                               the-string ((bu/string-generator 20))
                               the-fn (rand-nth [(fn encrypt-random-map [ch]
                                                   (get char-map ch))
                                                 (fn encrypt-random-limited-map [ch]
                                                   (get char-map-with-limited-values ch))
                                                 (fn encrypt-caesar [ch]
                                                   (nth available-chars
                                                        (mod (+ offset (.indexOf available-chars ch))
                                                             (count available-chars))))])
                               output (apply str (map the-fn the-string))]
                           {:inputs [the-string the-fn]
                            :output output}))
   "get-vals-of-key" (fn get-vals-of-key-gen
                       []
                       (let [num-keys-per-map (c/rand-int-range 1 8)
                             keys (repeatedly num-keys-per-map (bu/string-generator 10))
                             map-gen #(zipmap keys (repeatedly (bu/int-generator 1000)))
                             the-maps (c/rand-vector 0 25 map-gen)
                             the-key (rand-nth keys)
                             output (mapv #(get % the-key) the-maps)]
                         {:inputs [the-maps the-key]
                          :output output}))})

(defn extract-from-cases
  [m initial]
  (into []
        (keep (fn [[k v]]
                (when (and (keyword? k)
                           (.startsWith (name k) initial))
                  v))
              m)))
(defn extract-io
  [m initial]
  (map #(extract-from-cases % initial) m))

(defn test-results-with-model
  "Generates n programs for a given PSB2 problem and prints the expected
     output, actual output, and if they are the same"
  ([problem num-programs model suite-ns verbose]
   (let [prompt (probmap/get-desc (str problem))
         _ (println "prompt" prompt)
         programs (repeatedly num-programs
                              #(namespace-qualify-macros (extract-triple-backtick-code 
                                 (make-program-prompt-model (str "Main Task: Code an expert-level Clojure function that solves the given programming problem 
                                                                 without any comment, explanation, or example usage. Only return a single function.
                                                                 
                                                                 This function should follow these restrictions:
                                                                 Helper functions must be local, but try to avoid using them
                                                                 Do not use recursion
                                                                 Vectors, sets, the keys for maps, and the values of maps must contain a single type
                                                                 Cannot use these functions: some, recur, loop, when, letfn
                                                                 
                                                                 The problem:" prompt) model)))
         ex (if (= 'psb suite-ns)
              (get (psb2/fetch-examples "path/to/PSB2/datasets/" (str problem) 50 0) :train)
              (repeatedly 50 (get case-generators (str problem)))) 
         _ (println "Examples" ex)
         inputs (extract-io ex "input")
         _ (println "IN:" inputs)
         outputs (extract-io ex "output")
         _ (println "OUT:" outputs)]
      ;(println "IN: " inputs)
     (doseq [prog programs]
       (try
         (println "---------\n" prog)
         (if  (or (string/includes? prog "print") (string/includes? prog "println"))
           (let [answers (pmap #(string/replace (string/trim-newline
                                                 (with-out-str
                                                   (apply
                                                    (eval
                                                     (read-string prog))
                                                    (vec %))))
                                                "\r" "") inputs)]
             (when verbose (println answers))
             (when verbose (println outputs))
             (if (number? (first answers))
               (println (map #(<= (Math/abs (- %1 %2)) 0.00001) outputs answers))
               (println (= outputs answers))))
           (let [answers (mapv #(apply (eval (read-string prog)) (vec %)) inputs)]
             (when verbose (println "Actual: " answers))
             (when verbose (println "Expected: " outputs))
             (println (= outputs answers))))
         (catch Exception e (println (str "caught exception: " (.getMessage e)))))))))

(defn input-test
  [{:keys [problem model suite-ns]}]
  (println "Problem" problem)
  (test-results-with-model problem 10 model suite-ns true))

(defn llm-genome
  [{:keys [problem model verbose]}]
  (when verbose (println (type problem)))
  (let [prompt (probmap/get-desc (str problem))
        _ (when verbose (println "Prompt:" prompt))
        program-string (namespace-qualify-macros (extract-triple-backtick-code
                                                  (make-program-prompt-model (str "Main Task: Code an expert-level Clojure function that solves the given programming problem 
                                                                                 without any comment, explanation, or example usage. Only return a single function.
                                                                                 
                                                                                 This function should follow these restrictions:
                                                                                 Helper functions must be local, but try to avoid using them
                                                                                 Do not use recursion
                                                                                 Vectors, sets, the keys for maps, and the values of maps must contain a single type
                                                                                 Cannot use these functions: some, recur, loop, when, letfn
                                                                                 
                                                                                 The problem:" prompt) (str model))))
        _ (when verbose (println "Program-str" program-string))
        program-fn (read-string program-string)
        _ (when verbose (println "Program-fn" program-fn))]
    (try
      (let [decompiled-func (decompile/decompile-ast (ana.jvm/analyze program-fn))]
        (when verbose (println decompiled-func))
        decompiled-func)
      (catch Exception e (str "Failed Decompile:" (.getMessage e)))))
  )
(defn get-examples
  [{:keys [problem]}]
  (println (get (psb2/fetch-examples "data/psb/datasets" (str problem) 50 0) :train)))


(comment 
  (llm-genome {:problem "area-of-rectangle" :model "codestral"})
  (test-results-with-model "min-key" 10 "codestral" "composite" true)
  )
