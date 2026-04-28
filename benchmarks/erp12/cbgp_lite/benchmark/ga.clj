(ns erp12.cbgp-lite.benchmark.ga
  "Genetic algorithm run loop: lexicase selection, UMAD mutation, population evaluation, and post-run simplification."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [erp12.cbgp-lite.benchmark.problems :refer [problems genes-for-problem]]
            [erp12.cbgp-lite.benchmark.utils :as u]
            [erp12.cbgp-lite.genome :as g]
            [erp12.cbgp-lite.individual :as i]
            [erp12.ga-clj.search.ga :as ga]
            [erp12.ga-clj.toolbox :as tb]))

(def default-config
  {:n-train              200
   :n-test               2000
   :downsample-size      nil
   :population-size      1000
   :max-generations      300
   :umad-rate            0.4
   :min-genome-size      500
   :max-genome-size      2500
   :simplification-steps 2000})
   ;; Hooks are called on corresponding events and (probably) perform side effects (logging, update atoms, etc).
   ;; :hooks {:push (fn [ast state] ...)}

(defn make-breed
  "Creates a breed function that takes an evolutionary state and returns a child genome."
  [opts]
  (let [select (tb/make-lexicase-selection opts)
        mutate (tb/make-size-neutral-umad (assoc opts :rate (:umad-rate opts)))]
    (fn breed [state]
      (let [rand-parent (fn rand-parent []
                          (-> state
                             ;; Take 1 individual per error vector.
                              (->> (:grouped) (vals) (map rand-nth))
                             ;; Select a parent and mutate to child
                              (select state)
                              :genome))]
        (mutate (rand-parent))))))

(defn run
  [opts]
  (log/info "Options:"
            (->> opts
                 (map (fn [[k v]] (str (pr-str k) "\t" (pr-str v))))
                 (str/join "\n")
                 (str "\n")))
  (let [problem-name          (:problem opts)
        _                     (when (not (contains? problems problem-name))
                                (throw (ex-info (str "Unknown problem " problem-name) {:problems (keys problems)})))
        problem-metadata      (problems problem-name)
        config                (merge default-config problem-metadata opts)
        _                     (log/info "Type constructors:" (mapv :sym (:type-ctors config)))
        genes                 (genes-for-problem config)
        _                     (log/info "Genes:" (mapv (fn [gene] (or (:sym gene) (:value gene) gene)) genes))
        genetic-source        (g/make-genetic-source genes)
        genome-factory        (fn rand-genome [] (g/random-plushy-genome (assoc config :genetic-source genetic-source)))
        {:keys [train test]}  ((:dataset-reader problem-metadata) config)
        _                     (log/debug "Training cases:" train)
        num-errors            (let [num-cases (or (:downsample-size config) (:n-train config))]
                                (+ (* num-cases (count (:loss-fns config)))
                                   (if (nil? (:stdout-key config)) 0 num-cases)))
        ;; Give all training cases to the evaluator in case downsampling is not used.
        evaluator             (i/make-genome-evaluator (assoc config :cases train))
        {:keys [best result]} (ga/run {:population-size (:population-size config)
                                       ;; A nullary function that produces a genome for initializing the population.
                                       :genome-factory  genome-factory
                                       ;; A func called once per generation directly before evaluation.
                                       ;; Most importantly, it creates a sample of training cases to evaluate the population on this generation.
                                       ;; Also useful for logging and initializing values for stats collection.
                                       :pre-eval        (fn pre-eval [{:keys [step]}]
                                                          (log/info "STARTING" step)
                                                          {:cases      (if (:downsample-size config)
                                                                         (u/sample-n (:downsample-size config) train)
                                                                         train)
                                                           :step-start (System/currentTimeMillis)})
                                       ;; A function that accepts genomes and returns maps of fully evaluated individuals with behaviors and error vectors.
                                       :evaluator       evaluator
                                       ;; A func called once per generation directly after evaluation.
                                       ;; Most importantly, it groups individuals by their error vectors for lexicase pre-selection.
                                       ;; Also gather population statistics for logging.
                                       :post-eval       (fn post-evel [{:keys [individuals]}]
                                                          (doseq [[stat-name stat-val]
                                                                  (sort-by key
                                                                           (u/aggregate-stats {:code-depth            u/code-depth-stat
                                                                                               :code-depth-over-size  u/code-depth-over-size-stat
                                                                                               :code-size             u/code-size-stat
                                                                                               :exceptions            u/exception-messages-stat
                                                                                               :genome-size           u/genome-size-stat
                                                                                               :lowest-error-per-case u/lowest-error-per-case
                                                                                               :num-no-ast            u/num-no-ast-stat
                                                                                               :num-penalties         (u/make-num-penalty-stat (:penalty config))
                                                                                               :num-throwing          u/num-throwing-stat
                                                                                               :num-exceed-mem-guard  u/exceed-mem-guard-stat
                                                                                               :total-error           u/total-error-stat
                                                                                               :unique-behaviors      u/unique-behaviors-stat}
                                                                                              individuals))]
                                                            (log/info stat-name stat-val))
                                                          {:grouped (group-by :errors individuals)})
                                       ;; A function that takes the evolutionary state (after :post-eval) and returns a new child genome.
                                       :breed           (make-breed (assoc config
                                                                           :genetic-source genetic-source
                                                                           :genome-factory genome-factory
                                                                           :num-errors num-errors))
                                       ;; A comparator function used to track the "best" individual seen during evolution.
                                       :individual-cmp  (comparator #(< (:total-error %1) (:total-error %2)))
                                       ;; A function called once per generation to determine if evolution should stop.
                                       ;; Typically enforces some evaluation budget.
                                       ;; Can also be used for logging.
                                       :stop-fn         (fn stop? [{:keys [step step-start best new-best? individuals]}]
                                                          (log/info :best-individual-errors (:errors best))
                                                          (log/info "REPORT"
                                                                    {:step       step
                                                                     :duration   (- (System/currentTimeMillis) step-start)
                                                                     :best-error (:total-error best)
                                                                     :best-code  (:code best)})

                                                          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                          ;; RESEARCH - Save population snapshots (genomes) ever 50 generations
                                                          (when (zero? (mod step 50))
                                                            (let [file (io/file (format "data/snapshots/experiment=%s/problem=%s/trial=%s/gen=%s/genomes.edn"
                                                                                        "large-genomes" problem-name (:trial config) step))]
                                                              (io/make-parents file)
                                                              (spit file (pr-str (mapv :genome individuals)))))
                                                          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                                          (cond
                                                            (= step (:max-generations config)) :max-generation-reached
                                                                                                                    ;; If the "best" individual has solved the subset of cases
                                                                                                                    ;; Test if on the full training set.
                                                            (zero? (:total-error best))
                                                            (if (and new-best? (zero? (:total-error (evaluator (:genome best) {:cases test}))))
                                                              :solution-found
                                                                                                                        ;; If an individual solves a batch but not all training cases,
                                                                                                                        ;; no individual can become the new best and the run will fail.
                                                                                                                        ;; @todo Fix this in ga-clj somehow?
                                                              (log/error "Best individual solved a batch but not all test cases."))))
                                       ;; A map-like function for applying operations across the population of individuals (ie. evaluation).
                                       ;; Use pmap for parallelism (clj only)
                                       ;; Use mapv for cljs or during debugging to avoid laziness.
                                       :mapper          pmap})
        _                     (log/info "PRE-SIMPLIFICATION" (:code best))
        ;; Simplify the best individual seen during evolution.
        best                  (i/simplify {:individual           best
                                           :simplification-steps (:simplification-steps config)
                                           :evaluator            evaluator})
        _                     (log/info "POST-SIMPLIFICATION" (:code best))
        ;; Evaluate the final program on the unseen test cases.
        {:keys [solution?]}   (i/evaluate-func {:func     (:func best)
                                                :cases    test
                                                :loss-fns (:loss-fns problem-metadata)
                                                ;; Any positive number penalty works for the generalization test.
                                                :penalty  1})]
    (log/info "BEST CODE" (:code best))
    (if (= :solution-found result)
      (do
        (log/info "SOLUTION FOUND")
        (if solution?
          (log/info "SOLUTION GENERALIZED")
          (log/info "SOLUTION FAILED TO GENERALIZE")))
      (log/info "SOLUTION NOT FOUND"))
    (:func best)))

(comment

  (run {:problem  "fuel-cost"
        :data-dir "data/psb"})

  *e

  ;; Command
  ;; clj -X:benchmarks erp12.cbgp.benchmark.ga/run \
  ;;   :problem '"fuel-cost"' \
  ;;   :data-dir '"data/psb"' \
  ;;   :umad-rate 0.1 \
  ;;   :trial 1 

  (format "/data/problem=%s/rate=%s/" "fuel-cost" 0.2)

  (comment))
