(ns brain.core
  (require [brain.tree :as tree]
           [brain.function :as function]
           [brain.utils :as utils]
           [brain.numbers :as numbers]
           [brain.types :as types]))

(defn log
  [name l]
  (println name l)
  l)

(defn evaluate
  "Assign a score for each tree of the population"
  [population fitness]
  (map #(list (fitness %) %)
       population))

(defn sort-by-fitness
  "Sort a population based on the evaluation outputs"
  [fitness population]
  (let [evaluation (evaluate population fitness)]
    (map second (reverse (sort-by first evaluation)))))

(defn select
  "Tournament Selection.
   Selects some random individuals and keep the best one"
  [pop fitness tournament-size]
  (let [size (count pop)]
    (nth pop
         (apply max (repeatedly tournament-size #(rand-int size))))))

(defn mutate-population
  "Mutate a whole population based on a return-type
   and a mutation rate"
  ([params return-type population]
   (mutate-population params population return-type 0.5))
  ([params population return-type rate]
   (cons (first population)
         (map #(if (< (rand) rate)
                   (tree/mutate params % return-type)
                   %)
              (drop-last population)))))

;; with elite selection
(defn new-population
  "Returns a new population containing
   the best individual from the previous
   generation and some mutants. The population
   size should remains the same (it should right ?)"
  [params return-type fitness pop]
  (let [size (count pop)
        elite-count (* 1/4 size)
        mutant-count (* 3/4 size)
        tournament-size 50]
    (let [elite (take elite-count pop)
          mutants (repeatedly mutant-count
                              #(tree/mutate params (rand-nth elite) return-type))]
        (concat elite mutants))))


(defn evolve
  "Make one evolution iteration.
   Returns the new population"
  [params population fitness return-type]
  (->> population
       (sort-by-fitness fitness)
      ;  (mutate-population params return-type)
       (sort-by-fitness fitness)
       (new-population params return-type fitness)
       (sort-by-fitness fitness)))

(defn make-fitness
  "Takes a typed parameter vector and a function
   and return the fitness function, that takes a tree and apply
   a function to it"
  [params function]
  (fn [tree]
    (let [func (function/tree-to-function tree params)]
      (function func))))

(defn init
  "Initialize a genetic environment
   Return a lazy sequence of generations"
  [fitness return-type param size depth]
  (let [population (tree/make-population param size return-type depth)
        fitness-function (make-fitness param fitness)
        evolution #(evolve
                      param
                      (sort-by-fitness fitness-function %)
                      fitness-function
                      return-type)]
    (iterate evolution population)))

;; Init a genetic algorithm problem:

;; Define the inputs
(def input (range 10))

;; define the coresponding outputs
;; 2xx + 5x + x
(def output (map #(+ (* % % 2) (* % 5)) input))

;; define a fitness function. The better the program the higher the score.
;; Cannot go over 0 (shoul it?)
(defn my-fitness
  [function]
  (let [got (map function input)]
    (let [vector (map #(utils/score %2 %1) output got)]
      (apply + vector))))

;; Defines the environment
;; returns a lazy sequence of populations
(def gen (init  my-fitness
                numbers/gen-number
                [numbers/gen-number 'x]
                150
                5))

(defn run
  "Takes a lazy-seq of evolving populations
   and recur until depth is 0 or the goal has been reached"
  [genome depth]
  (let [population (first genome)
        evaluation (evaluate population (make-fitness [numbers/gen-number 'x] my-fitness))]
    (println (first (map first (reverse (sort-by first evaluation)))))
    (if (or
          (> 1 depth)
          (= 0 (first (map first evaluation))))
        (first population)
        (run (rest genome) (dec depth)))))


(defn -main
  []
  (println "result" (utils/pretty-tree (run gen 1000))))
