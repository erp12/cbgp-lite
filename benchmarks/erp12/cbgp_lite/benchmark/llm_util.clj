(ns erp12.cbgp-lite.benchmark.llm-util
  (:require clojure.pprint
            [clojure.string :as string]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [erp12.cbgp-lite.benchmark.suite.composite :as c]
            [erp12.cbgp-lite.benchmark.suite.psb :as psb]
            [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.compile :as compi]
            [erp12.cbgp-lite.lang.decompile :as decompile]
            [erp12.cbgp-lite.lang.llm.problem-map :as probmap]
            [erp12.cbgp-lite.search.individual :as i]
            [erp12.cbgp-lite.task :as task]
            [pyjama.core :as ollama]
            [taoensso.timbre :as log]
            [taoensso.timbre.appenders.core :as log-app]
            
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rt]))

(log/merge-config!
 {:output-fn (partial log/default-output-fn {:stacktrace-fonts {}})
  :appenders {:println (assoc (log-app/println-appender) :min-level :info)}})

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

(defn safe-read-string
  "Ensures that read-string can't use eval macros like #=(+ 1 2) to evaluate"
  [s]
  (binding [*read-eval* false]
    (read-string s)))

(defn extract-triple-backtick-code
  "Uses a regex to grab all text between first two triple backticks. Ignores
   language specifier after first triple backticks, if it exists."
  [s]
  (let [pattern #"(?s)```(?:\w+\n)?(.*?)```"
        matches (map second (re-seq pattern s))]
    (if (seq matches)
      (first matches)
      s)))

(defn extract-paren-to-blank-line 
  "Extracts first parenthesis in s up to either first blank line or end of string.
   If s does not have an open paren, returns s"
  [s]
  (if-let [match (re-find #"(?s)\(.*?(?:\n[ \t]*\n|$)" s)]
    match
    s))

(defn extract-first-parens-and-pad-end
  "Returns substring containing first pair of matching parentheses.
   If no open paren, returns whole string.
   Adds parentheses to end of string, likely more than necessary. Since this string
   is always run through read-string, which should ignore extra parens, this is fine."
  [s]
  (let [start-to-end (extract-paren-to-blank-line s)
        num-open-parens (get (frequencies start-to-end) \()]
    (apply str start-to-end (repeat num-open-parens \) ))))

(defn namespace-qualify-macros
  "Ensures that macros are namespace qualified correctly as functions."
  [s]
  (-> s
      (clojure.string/replace "(and " "(erp12.cbgp-lite.lang.lib/and ")
      (clojure.string/replace "(or " "(erp12.cbgp-lite.lang.lib/or ")))

(defn clean-llm-program-string
  "Given LLM output, finds and cleans the first function in triple backquotes."
  [prog-string]
  (-> prog-string
      extract-triple-backtick-code
      extract-first-parens-and-pad-end
      namespace-qualify-macros))

(defn llm-generate-program-string
  "Generates and cleans a program string using given model and prompt"
  [prompt model]
  (clean-llm-program-string (make-program-prompt-model prompt model)))

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

Below are two example uses of this function along with the correct output:
```clojure
(#problem-name# #example-inputs1#) => #example-output1#

(#problem-name# #example-inputs2#) => #example-output2#
```
Here is the function to implement:
```clojure
(defn solve-#problem-name#
  \"#problem-description#
   This function outputs a #output-type#.
   \"
  [#parameter-list#]
   ; your code here
)
```
")

(defn get-problem-info
  "Grabs the problem info map from composite or PSB to help build prompt"
  [problem]
  (cond
    (contains? c/composite-problems problem) (get (c/problems 10e9) problem)
    (contains? psb/psb-problems problem) (get (psb/problems 10e9) problem)
    :else (throw (Exception. (str "Problem not found: " problem)))))

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
  "Helper; turns a list into a string of those things"
  [lst]
  (apply pr-str lst))

(defn get-parameters-str
  "From a problem-info map, extract the parameters into a string"
  [problem-info]
  (list->string (keys (:input->type problem-info))))

(defn build-prompt
  "Makes the final prompt to give to the LLM"
  [{:keys [problem train]}]
  (let [problem (str problem)
        problem-description (probmap/get-desc problem)
        problem-info (get-problem-info problem)
        train-examples (take 2 (shuffle train))
        inputs1 (list->string (:inputs (first train-examples)))
        output1 (pr-str (:output (first train-examples)))
        inputs2 (list->string (:inputs (second train-examples)))
        output2 (pr-str (:output (second train-examples)))]
    (-> core-prompt
        (clojure.string/replace "#problem-name#" problem)
        (clojure.string/replace "#problem-description#" problem-description)
        (clojure.string/replace "#output-type#" (get-output-type-str problem-info))
        (clojure.string/replace "#parameter-list#" (get-parameters-str problem-info))
        (clojure.string/replace "#example-inputs1#" inputs1)
        (clojure.string/replace "#example-inputs2#" inputs2)
        (clojure.string/replace "#example-output1#" output1)
        (clojure.string/replace "#example-output2#" output2))))

(defn generate-llm-program-strings
  "Generates programs using LLM"
  [{:keys [model number-llm-programs]
    :as opts}]
  (let [full-prompt (build-prompt opts)
        _ (log/info "Prompt:" full-prompt)
        _ (log/info "LLM Generating" number-llm-programs "solution programs.")
        llm-program-strings (repeatedly number-llm-programs
                                        #(llm-generate-program-string full-prompt model))]
    llm-program-strings))

(defn convert-llm-string-to-fn
  "Convert LLM-generated string of a function into a function.
   If fails, returns nil"
  [fn-string]
  (try
    (eval (safe-read-string fn-string))
    (catch Exception _ nil)))

(defn evaluate-llm-program-strings
  "Turns each program in llm-generated-program-strings into a program and evaluates it.
   Returns a list of all programs that pass all training cases, which will be empty
   if none do."
  [llm-generated-program-strings {:keys [evaluate-fn train] :as opts}]
  (let [evaluated-inds (map #(let [func (convert-llm-string-to-fn %)]
                               (assoc (evaluate-fn (assoc opts
                                                          :cases train
                                                          :func func))
                                      :code-string %
                                      :code (try (safe-read-string %)
                                                 (catch Exception _ nil))
                                      :func func))
                            llm-generated-program-strings)
        _ (log/info "Number of LLM programs that parse without exception:" (count (filter #(not (nil? (:code %)))
                                                                                          evaluated-inds)))
        _ (log/info "Number of LLM programs that run without exception:" (count (filter #(and (not (nil? (:code %)))
                                                                                              (not (nil? (:behavior %)))
                                                                                              (nil? (:exception %)))
                                                                                        evaluated-inds)))]
    (filter :solution? evaluated-inds)))

(defn decompile-llm-program-strings-to-genomes
  "Decompiles each LLM-generated program string into a genome, if possible, and
   returns those that decompiled successfully."
  [llm-generated-program-strings]
  (remove nil?
          (map (fn [program-string]
                 (try
                   (-> program-string
                       safe-read-string
                       ana.jvm/analyze
                       decompile/decompile-ast)
                   (catch Exception _ nil)))
               llm-generated-program-strings)))

(defn check-compilation-of-decompiled-genomes
  "Takes decompiled LLM genomes and attempts to recompile them, only returning
   those that properly recompile."
  [genomes evaluator]
  (filter (fn [genome]
            (try
              (let [evalled (evaluator genome {})]
                (println "evaluated genome:" (dissoc evalled :behavior)) ;; TMH remove later
                evalled)
              (catch Exception _ nil)))
          genomes))

(def default-config
  {:n-train              200
   :n-test               2000
   ;; Lower population size for easier testing 
   :population-size      10
   :max-generations      300
   :umad-rate            0.1
   :min-genome-size      50
   :max-genome-size      250
   :penalty              1e6
   :simplification-steps 2000
   :data-dir             "data/psb"
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Experimental
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Supported -  nil, :biggest, :newest, or a function from state to unboxed AST.
   ;; `nil` will search the stack for the top AST of a valid type.
   :state-output-fn      nil
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; LLM-GP
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   :model "llama3.2" ; TMH: Replace with "codestral" for use on HPC
   :number-llm-programs 0 ;; TMH: set back to 50
   })

(defn run
  "Just for testing with hand-written programs in place of llm-generated-program-strings"
  [{:keys [type-counts-file] :as opts} llm-generated-program-strings]
  (log/info "Options:"
            (->> opts
                 (map (fn [[k v]] (str (pr-str k) "\t" (pr-str v))))
                 (string/join "\n")
                 (str "\n")))
  ;opts is parameters
  (when (:app-type opts)
    (reset! compi/app-type (:app-type opts)))
  (when (:baked-in-apply-probability opts)
    (reset! compi/baked-in-apply-probability (:baked-in-apply-probability opts)))
  (when (:backtracking opts)
    (reset! compi/backtracking (:backtracking opts)))
  (when type-counts-file
    (log/warn "Type counting enabled. This is slow!")
    (reset! compi/collect-types? true)
    (reset! compi/types-seen {}))
  (let [config (merge default-config opts)
        task (-> config
                 bu/read-problem
                 task/enhance-task
                 (assoc :evaluate-fn i/evaluate-full-behavior))
        opts (merge config task)
        _ (log/info "Type Constructors: " (:type-ctors opts))
        _ (log/info "Vars:" (:vars opts))
        evaluator (i/make-evaluator (-> opts
                                        (assoc :cases (:train task))
                                        (dissoc :train :test)))
        gen-start-time (System/nanoTime)
        ;; llm-generated-program-strings (generate-llm-program-strings opts)
        _ (log/info "LLM-generated program strings:")
        _ (doseq [prog-string llm-generated-program-strings]
            (println prog-string)
            (println))
        _ (log/info "Time for LLM to generate" (:number-llm-programs opts) "programs (in ms):" (/ (- (System/nanoTime) gen-start-time) 1e6))
        llm-solutions (evaluate-llm-program-strings llm-generated-program-strings opts)
        ;llm-solutions '() ;; TMH remove later, just to make it so we always try to enter GP
        factory (let [llm-genomes-decompiled (decompile-llm-program-strings-to-genomes llm-generated-program-strings)
                      _ (log/info "Number of decompiled LLM programs (may not properly recompile):" (count llm-genomes-decompiled))
                      llm-genomes (vec (check-compilation-of-decompiled-genomes llm-genomes-decompiled evaluator))
                      _ (log/info "Number of decompiled LLM programs used as seeds for GP genomes:" (count llm-genomes))
                      _ (println "GENOMES TMH")
                      _ (clojure.pprint/pprint llm-genomes)]

                  (if (empty? llm-genomes)
                    (log/info "No LLM genomes, so using random genomes to seed population.")
                    #(rand-nth llm-genomes)))]
    ;; (println "type-env keys:" (keys (:type-env opts)))
    ;; (println "flatten in type-env" (get (:type-env opts) 'flatten))

    #_(map #(dissoc % :behavior :exception) llm-solutions)
    factory))








(comment
  

  (decompile-llm-program-strings-to-genomes '("(defn what
  [input1]
  [1 2 3])"))

  (decompile-llm-program-strings-to-genomes
   '("(defn what
    [input1]
    (vector 2 3 input1 6))"))
  ;;=> (({:gene :lit, :val 6, :type {:type int?}}
  ;;     {:gene :local, :idx 0}
  ;;     {:gene :lit, :val 3, :type {:type int?}}
  ;;     {:gene :lit, :val 2, :type {:type int?}}
  ;;     {:gene :var, :name ->vector1}
  ;;     {:gene :apply}))
  
  (run {:suite-ns 'erp12.cbgp-lite.benchmark.suite.composite
        :problem "sets-with-element"}
       (list prog-with-flatten
             prog-with-set))

  (def prog-with-flatten "(defn sets-with-element
  [input1 input2]
  (set (flatten input1)))")

  (def prog-with-set "(defn sets-with-element
    [input1 input2]
    (disj input1 #{}))")

  (run {:suite-ns 'erp12.cbgp-lite.benchmark.suite.composite
        :problem "area-of-rectangle"}
       '("(defn its-a-fn [x] (+ x 5))"
         "(defn [x] (+ x 5))"
         "(defn not-enough-parens [x] (+ x 5"
         "(defn yay-solution [[x1 y1] [x2 y2]] (* (- x2 x1) (- y2 y1)))"
         "(defn not-solution [[x1 y1] [x2 y2]] (+ (- x2 x1) (- y2 y1)))"))

  (map clean-llm-program-string
       '("(defn its-a-fn [x] (+ x 5))"
         "(defn [x] (+ x 5))"
         "(defn not-enough-parens [x] (+ x 5"
         "(defn too-many-parens [x] (+ x 5)))))"
         "(defn yay-solution [[x1 y1] [x2 y2]] (* (- x2 x1) (- y2 y1)))"
         "(defn not-solution [[x1 y1] [x2 y2]] (+ (- x2 x1) (- y2 y1)))"
         "(defn parens-in-docstring-and-char \"Paren in docstring ) <- there \" [x] (str \\) x))"))


  ((convert-llm-string-to-fn "(defn itsafn [x] (+ x 5))")
   100)
  ;;=> 105
  
  (convert-llm-string-to-fn "(defn itsafn [x] (+ x 5")

  (convert-llm-string-to-fn "(defn [x] (+ x 5))")

  (clojure.pprint/pprint
   (sort '(:description :penalty :suite-ns :simplification-steps :state-output-fn :num-errors :max-genome-size :case-generator :genetic-source :problem :train :loss-fns :max-generations :data-dir :arg-symbols :genome-factory :other-type-ctors :min-genome-size :n-test :n-train :evaluate-fn :umad-rate :type-ctors :number-llm-programs :type-env :ret-type :input->type :extra-genes :test :population-size :dealiases :vars :model)))

  (llm-generate-program-string "Write a Clojure program that determines whether a given string contains any vowels."
                               "llama3.2:latest")

  (get-problem-info "filter-bounds")

  (repeatedly 5 (:case-generator (get-problem-info "simple-encryption")))

  (println
   (build-prompt {:problem "area-of-rectangle"
                  :train '({:inputs [[84.46441553605138 58.03525828694745] [56.86341183312811 -86.5646160858711]], :output 3991.101668006403} {:inputs [[99.20429281101187 44.57268987878183] [60.51958748043097 -99.92651173185263]], :output 5589.909034811594} {:inputs [[77.71616722922556 80.57726215470203] [23.95803183732876 -63.81297012313281]], :output 7762.149656059272} {:inputs [[-47.46796683738652 87.22753517151466] [-87.36570031974694 8.952953322082323]], :output 3122.978405071857} {:inputs [[76.60755064800512 64.45702535466398] [-72.45161220096614 -39.54152070648034]], :output 15501.93621338435} {:inputs [[30.153442023085972 27.806408008782185] [21.093630971300655 -44.44849954565848]], :output 654.615810007448} {:inputs [[-56.30262627057736 75.91596781329656] [-94.94132052533269 -50.24515676065309]], :output 4874.701119248939} {:inputs [[95.65518979318767 83.4441371194612] [30.2112041369395 -49.63866435094517]], :output 8709.468950522598} {:inputs [[60.620130143297985 59.72022354354377] [-47.522181130905004 -71.92982984068625]], :output 14236.941052342845} {:inputs [[99.45318392082149 -51.21387159252255] [-48.12408904518246 -76.55630393069382]], :output 3739.967054792786} {:inputs [[84.97307069674022 -4.826930700284237] [46.153160249446245 -21.46155198756705]], :output 645.7545086969689} {:inputs [[47.36237280491483 45.426956401407466] [41.7883961511956 -74.33796758724283]], :output 667.5668902471947})}))

  (println
   (build-prompt {:problem "simple-encryption"
                  :train (repeatedly 5 (:case-generator (get-problem-info "simple-encryption")))}))
  
  (println
   (build-prompt {:problem "min-key"
                  :train (repeatedly 5 (:case-generator (get-problem-info "min-key")))}))


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
   (llm-generate-program-string
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
        (vals psb/psb-problems)))

  )