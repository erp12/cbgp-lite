(ns erp12.cbgp-lite.benchmark.llm-util-old
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [erp12.cbgp-lite.benchmark.suite.composite :as c]
            [erp12.cbgp-lite.benchmark.suite.psb :as psb]
            [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.decompile :as decompile]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.llm.problem-map :as probmap]
            [psb2.core :as psb2]
            [pyjama.core :as ollama]
            [taoensso.timbre :as log]))  
 
(defn make-program-prompt-model
  "Given a prompt and a model, uses ollama to generate a response.
   Note: for newer versions of pyjama, we will need to remove with-out-str,
   since newer pyjama returns a string instead of printing it."
  [prompt model]
  (with-out-str
    (ollama/ollama
     "http://localhost:11434"
     :generate
     {:prompt prompt
      :model model})))

(defn extract-triple-backtick-code
  "Uses a regex to grab all text between first two triple backticks. Ignores
   language specifier after first triple backticks, if it exists."
  [s]
  (let [pattern #"(?s)```(?:\w+\n)?(.*?)```"
        matches (map second (re-seq pattern s))]
    (if (seq matches)
      (first matches)
      s)))

(defn namespace-qualify-macros
  "Ensures that macros are namespace qualified correctly as functions."
  [s]
  (-> s
      (clojure.string/replace "and " "erp12.cbgp-lite.lang.lib/and ")
      (clojure.string/replace "or " "erp12.cbgp-lite.lang.lib/or ")))

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

(def core-prompt
"You are a smart Clojure programmer implementing a function based on its docstring description. 
Alter the following Clojure function so that it implements its docstring. 
Only return this single function. Do not explain the function.
Do not include print statements, in-line comments, multi-line comments, or unit tests. 
Put the resulting function between triple backticks.

This function should follow these restrictions:
1. Do not use helper functions unless they are locally defined using let.
2. Do not use recursion
3. Only use base Clojure methods.
4. Vectors, sets, the keys for maps, and the values of maps must contain a single type
5. Cannot use these functions: some, recur, loop, when, letfn

```clojure
(defn #problem-name#
  \"#problem-description#
   This function outputs a #output-type#.
   Below are two example uses of this function along with the correct output:
   (#problem-name# #example-inputs1#) => #example-output1#
   (#problem-name# #example-inputs2#) => #example-output2#
   \"
  [#parameter-list#]
   ; your code here
)
```
")

(defn get-problem-info
  "Grabs the problem info map from composite or PSB"
  [problem]
  (cond
    (contains? c/composite-problems problem) (get (c/problems 10e9) problem)
    (contains? psb/psb-problems problem) (get (psb/problems 10e9) problem)
    :else (throw (Exception. "Problem not found"))))

(defn get-output-type-str
  [problem-info]
  (let [type-strings
        {{:type 'int?} "integer"
         {:type 'double?} "double"
         {:type 'string?} "string"
         {:type 'char?} "char"
         {:type 'boolean?} "boolean"
         {:type :s-var, :sym 'T} "any type"
         {:type :set, :child {:type 'int?}} "set of integers"
         {:type :vector, :child {:type 'int?}} "vector of integers"
         {:type :tuple, :children [{:type 'int?} {:type 'int?}]} "vector containing 2 integers"
         {:type :set, :child {:type :set, :child {:type 'int?}}} "set of sets of integers"
         {:type :set, :child {:type :s-var, :sym 'T, :typeclasses #{:comparable}}} "set of any type"
         {:type :set, :child {:type :tuple, :children [{:type 'int?} {:type 'int?}]}} "set of vectors containing 2 integers"
         {:type :tuple, :children [{:type :vector, :child {:type 'int?}} {:type :vector, :child {:type 'int?}}]} "vector containing 2 vectors of integers"}]
    (if (contains? type-strings (:ret-type problem-info))
      (get type-strings (:ret-type problem-info))
      "UNDEFINED TYPE")))

(defn list->string
  [lst]
  (apply str 
         (interpose " " lst)))

(defn get-parameters-str
  [problem-info]
  (list->string (keys (:input->type problem-info))))

(defn build-prompt
  "Makes the final prompt to give to the LLM"
  [{:keys [problem train]}]
  (let [problem-description (probmap/get-desc problem)
        problem-info (get-problem-info problem)
        train-examples (take 2 (shuffle train))
        inputs1 (list->string (:inputs (first train-examples)))
        output1 (str (:output (first train-examples)))
        inputs2 (list->string (:inputs (second train-examples)))
        output2 (str (:output (second train-examples)))]
    (-> core-prompt
        (clojure.string/replace "#problem-name#" problem)
        (clojure.string/replace "#problem-description#" problem-description)
        (clojure.string/replace "#output-type#" (get-output-type-str problem-info))
        (clojure.string/replace "#parameter-list#" (get-parameters-str problem-info))
        (clojure.string/replace "#example-inputs1#" inputs1)
        (clojure.string/replace "#example-inputs2#" inputs2)
        (clojure.string/replace "#example-output1#" output1)
        (clojure.string/replace "#example-output2#" output2))))

(defn test-results-with-model
  "Generates n programs for a given PSB2 problem and prints the expected
     output, actual output, and if they are the same"
  [problem num-programs model suite-ns verbose]
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
                                                                 
                                                                 The problem:" prompt) model))))
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
         (catch Exception e (println (str "caught exception: " (.getMessage e))))))))


(defn llm-genome
  "Generates LLM code and decompiles it into a plushy genome"
  [{:keys [problem model verbose]}]
  (let [prompt (probmap/get-desc problem)
        _ (when verbose (println "Prompt:" prompt))
        program-string (namespace-qualify-macros (extract-triple-backtick-code
                                                  (make-program-prompt-model (str "Main Task: Code an expert-level Clojure function that solves the given programming problem 
                                                                                 without any comment, explanation, or example usage. Only return a single function.
                                                                                 
                                                                                 This function should follow these restrictions:
                                                                                 Helper functions must be local, but try to avoid using them
                                                                                 Do not use recursion
                                                                                 Vectors, sets, the keys for maps, and the values of maps must contain a single type
                                                                                 Cannot use these functions: some, recur, loop, when, letfn
                                                                                 
                                                                                 The problem:" prompt) 
                                                                             model)))
        _ (when verbose (println "Program-str" program-string))
        program-fn (read-string program-string)
        _ (when verbose (println "Program-fn" program-fn))]
    (try
      (let [decompiled-func (decompile/decompile-ast (ana.jvm/analyze program-fn))]
        (when verbose (println decompiled-func))
        decompiled-func)
      (catch Exception e (str "Failed Decompile:" (.getMessage e))))))

(defn get-examples
  [{:keys [problem]}]
  (println (get (psb2/fetch-examples "data/psb/datasets" (str problem) 50 0) :train)))


(comment 
  (llm-genome {:problem "area-of-rectangle" :model "codestral"})
  (test-results-with-model "min-key" 10 "codestral" "composite" true) 

  (extract-triple-backtick-code "Here's some code:\n```clojure\ncode goes\nhere```\nand now another\n```python\ndef function yeah\nmore\n```\nstuff here.")

  (llm-genome {:problem "count-true" :model "llama3.2:latest" :verbose true})

  (clojure.string/replace "hello there world he he yay the woo" "he" "HE")

  
  (println
   (build-prompt {:problem "area-of-rectangle"
                  :train '({:inputs [[84.46441553605138 58.03525828694745] [56.86341183312811 -86.5646160858711]], :output 3991.101668006403} {:inputs [[99.20429281101187 44.57268987878183] [60.51958748043097 -99.92651173185263]], :output 5589.909034811594} {:inputs [[77.71616722922556 80.57726215470203] [23.95803183732876 -63.81297012313281]], :output 7762.149656059272} {:inputs [[-47.46796683738652 87.22753517151466] [-87.36570031974694 8.952953322082323]], :output 3122.978405071857} {:inputs [[76.60755064800512 64.45702535466398] [-72.45161220096614 -39.54152070648034]], :output 15501.93621338435} {:inputs [[30.153442023085972 27.806408008782185] [21.093630971300655 -44.44849954565848]], :output 654.615810007448} {:inputs [[-56.30262627057736 75.91596781329656] [-94.94132052533269 -50.24515676065309]], :output 4874.701119248939} {:inputs [[95.65518979318767 83.4441371194612] [30.2112041369395 -49.63866435094517]], :output 8709.468950522598} {:inputs [[60.620130143297985 59.72022354354377] [-47.522181130905004 -71.92982984068625]], :output 14236.941052342845} {:inputs [[99.45318392082149 -51.21387159252255] [-48.12408904518246 -76.55630393069382]], :output 3739.967054792786} {:inputs [[84.97307069674022 -4.826930700284237] [46.153160249446245 -21.46155198756705]], :output 645.7545086969689} {:inputs [[47.36237280491483 45.426956401407466] [41.7883961511956 -74.33796758724283]], :output 667.5668902471947})}))

  (println
   (build-prompt {:problem "filter-bounds"
                  :train '({:inputs [#{920 765 119 -570 817 -254 -863 -875 974 -488 -532 531 -479 846 111 285 -831 -46 918 -33 -373 696 -646 212} -662 996], :output #{920 765 119 -570 817 -254 974 -488 -532 531 -479 846 111 285 -46 918 -33 -373 696 -646 212}} {:inputs [#{"" "n" ",.(N" "Cbj" ">R" "\n]~" ",B&U'14" "U)]" "#(@S_ILa" "E|ZJ1rI" "._rd" "M" ":$wy" "[v4yp#1" "cj" "nC" "P0SsyH" "U4K9 W\t" "dUdAq<vd|" "1K\"D``" ";jr~" "|\t; N" "6g<pcR" "ZYM?(6" "l" "[" "I" "{Ax\"H\n" "SV3c&1" "/|pfa$1G" "'DF" "_yf1j1o" "$w6" "y/^u,w" "_,f/X8" "PETD&" "EH3ijm!H^"} "%nS" "@eQ-6\"s"], :output #{",.(N" ">R" ",B&U'14" "._rd" ":$wy" "1K\"D``" ";jr~" "6g<pcR" "/|pfa$1G" "'DF"}} {:inputs [#{"" "'" "r9" "6}%" "Ii>cM" "p" "MWzM+xz" "z;gztF" "M" "@&" "o}+wh,=Vs" "z]eG.qLs" "7" "LI61" "[__LW%0&" "7xg%y+" "F" ">~" ">7dO" "`:" "w|" "k" "\tTN@A0" "-sY" "qd]{" "jo\t=TU" "(S" "ub" "gr:KA"} "C\\=e%B<" "~73rHJu"], :output #{"r9" "Ii>cM" "p" "MWzM+xz" "z;gztF" "M" "o}+wh,=Vs" "z]eG.qLs" "LI61" "[__LW%0&" "F" "`:" "w|" "k" "qd]{" "jo\t=TU" "ub" "gr:KA"}} {:inputs [#{\B \b \# \D \% \E \& \G \( \i \* \J \, \L \M \m \N \O \q \R \3 \s \4 \t \5 \u \v \W \8 \x \9 \Y \y \z \[ \{ \| \~ \_} \_ \c], :output #{\b}} {:inputs [#{0.2878055754979152 0.5415513592977138 0.1325146460877925 0.4847502095489986 0.611385685606361 0.33714302513501015 0.8572575071892744 0.6815563103021534 0.9569830690624335 0.6181315333500541 0.5962827897611606 0.23393194282172702 0.10205556077315503 0.5116348959233826 0.9339611526702942 0.875806661099536 0.9698983811332761 0.5221263171566857 0.29658950619316504 0.3089713200460459 0.7695445194077073 0.05813814698934794 0.6563890719949719 0.9502085536616002 0.5317740489106225 0.7503835285740329 0.2631170147549001 0.00779354050882497} 0.28742091667015013 0.4037638526278703], :output #{0.2878055754979152 0.33714302513501015 0.29658950619316504 0.3089713200460459}} {:inputs [#{0.20368894555738382 0.5324633772658153 0.6070640649483201 0.05129925024077009 0.23554690108553178 0.8822888320708152 0.44844235535979493 0.8719475165185236 0.8980273128458767 0.7943974999095178 0.10149054443488259 0.9401792847800005 0.24684646357628193 0.7972094505710058 0.30044714028073716 0.8372441387217185 0.08895664211718013 0.9357586672248869 0.3477366912016818 0.25974627017836294 0.4506032696396839 0.6324705731228164 0.28586652897133025 0.5055557118516621 0.3487164092136764 0.3487289163849322 0.3472079535219005 0.5050866143724076 0.22974163939381986 0.796574676407359 0.7970601780775431 0.6442470458094776} 0.17871471027756936 0.6673037619185457], :output #{0.20368894555738382 0.5324633772658153 0.6070640649483201 0.23554690108553178 0.44844235535979493 0.24684646357628193 0.30044714028073716 0.3477366912016818 0.25974627017836294 0.4506032696396839 0.6324705731228164 0.28586652897133025 0.5055557118516621 0.3487164092136764 0.3487289163849322 0.3472079535219005 0.5050866143724076 0.22974163939381986 0.6442470458094776}} {:inputs [#{\d \e \F \f \' \* \, \l \O \P \1 \3 \S \T \t \W \8 \y \\ \| \= \]} \U \]], :output #{\W \\}} {:inputs [#{0 -12 7 20 -20 -2 -1 -6 6 -14 -7 -16 19 11 -9 -18 18 -5 8} -9 11], :output #{0 7 -2 -1 -6 6 -7 -5 8}} {:inputs [#{\@ \` \! \B \b \$ \D \% \F \g \j \K \k \, \M \N \n \/ \O \o \1 \2 \S \4 \5 \v \Y \: \; \< \> \^ \? \_} \A \R], :output #{\B \D \F \K \M \N \O}} {:inputs [#{\space \@ \` \A \a \B \# \C \c \% \E \& \f \g \h \tab \) \* \j \+ \L \M \. \N \O \0 \P \q \4 \U \u \V \X \y \; \{ \= \^} \4 \N], :output #{\@ \A \B \C \E \L \M \; \=}})}))

  (println
   (build-prompt {:problem "for-loop-index"}))

  (println
   (build-prompt {:problem "vectors-summed"
                  :train '({:inputs [[] []], :output []}
                           {:inputs [[0] [0]], :output [0]}
                           {:inputs [[0 0] [0 0]], :output [0 0]}
                           {:inputs [[0 1] [-4 2]], :output [-4 3]}
                           {:inputs [[-1 0] [-3 0]], :output [-4 0]}
                           {:inputs [[-90 -6] [-323 49]], :output [-413 43]})}))

  (println
   (build-prompt {:problem "basement"
                  :train '({:inputs [[2 -2 -1]], :output 2}
                           {:inputs [[5 -6 -5]], :output 1}
                           {:inputs [[5 -5 -5]], :output 2}
                           {:inputs [[100 -100 -100]], :output 2}
                           {:inputs [[-100 -100 -100]], :output 0}
                           {:inputs [[-1 100 99]], :output 0}
                           {:inputs [[5 -10 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5]], :output 1}
                           {:inputs [[5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 -99]], :output 19})}))

  (println
   (make-program-prompt-model
    (build-prompt {:problem "basement"
                   :train '({:inputs [[2 -2 -1]], :output 2}
                            {:inputs [[5 -6 -5]], :output 1}
                            {:inputs [[5 -5 -5]], :output 2}
                            {:inputs [[100 -100 -100]], :output 2}
                            {:inputs [[-100 -100 -100]], :output 0}
                            {:inputs [[-1 100 99]], :output 0}
                            {:inputs [[5 -10 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5]], :output 1}
                            {:inputs [[5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 -99]], :output 19})})
    "llama3.2:latest"))

  (get-problem-info "area-of-rectangle")
  (get-problem-info "for-loop-index")
  (get-problem-info "basement")
  (get-problem-info "whats")

  (sort-by count
           (map str
                (set
                 (concat
                  (map :ret-type (vals c/composite-problems))
                  (map :ret-type (vals psb/psb-problems))))))

  (map get-parameters-str
       (concat
        (vals c/composite-problems)
        (vals psb/psb-problems))))