(ns brain.core
  (require [clojure.zip :as zip]))

(def ideal (map #(vector % (* % 2)) (range -10 10)))

(defn add [a b] (+ a b))
(defn sub [a b] (- a b))
(defn mult [a b] (* a b))
(defn div [a b] (if (zero? b) 0 (/ a b)))

;; terminals can be constants or
;; unknown values. Here x
(def terminals '(x 0 1 2 3 4 5 6 7 8 9 10))
(def x 1)

;; Return a map describing a function
;; value = the actual function, return = the function return type
;; arguments = arguments type
;; arity = number of arguments
(defn register-function [return value arguments-vector]
  { :value value
    :return return
    :arguments arguments-vector
    :arity (count arguments-vector) })

;; Initial functions set
;; The testing one contains only integer specific operations
(def functions
  [ (register-function :integer `add  [:integer :integer])
    (register-function :integer `sub  [:integer :integer])
    (register-function :integer `mult [:integer :integer])
    (register-function :integer `div  [:integer :integer]) ])

;; Return a random terminal or fucntion of the previously defined set
(defn random-terminal [] (rand-nth terminals))
(defn random-function [] (rand-nth functions))

;; Create a Clojure tree
(defn build-tree [depth]
  (if (or (= depth 0)
          (< (rand) 0.5)) (random-terminal)
  (let [function (random-function)]
    (cons (:value function) (repeatedly (:arity function) #(build-tree (dec depth)))))))

(defn get-function [tree]
  (eval (list 'fn '[x] tree)))

(defn abs [n] (max n (- n)))

(defn distance [x y] (abs (- x y)))

(defn error [tree]
  (let [function (get-function tree)]
    (reduce + (map (fn [[input output]] (distance (function input) output)) ideal))))

(def depth 2)

(def tree (build-tree depth))
(println tree)

(defn -main []
  (println (error tree)))
