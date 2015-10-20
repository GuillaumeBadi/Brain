;; This is the Core namespace.
;; It requires walk, referrenced as 'w'
(ns brain.core
  (require [clojure.walk :as w]))

(defn get-types
  "Takes a vector [:number a :number b] and returns [:number :number]"
  [v] (into [] (filter keyword? v)))

(defn get-values
  "Takes a vector [:number a :number b] and returns [a b]"
  [v] (into [] (filter (complement keyword?) v)))

(defmacro defgn
  "Macro used to decalre typed functions"
  [t n args & body]
  (let [values (get-values args)
        typs (get-types args)]
        `(def ~n
            ^{
              :nm ~(name n)
              :typ ~t
              :takes ~typs
            }
            (fn ~n ~values ~@body))))

;;Some typed functions for the algorithm
(defgn :number add [:number a :number b] (+ a b))
(defgn :number mult [:number a :number b] (* a b))
(defgn :number div [:number a :number b] (if (zero? b) 0 (/ a b)))
(defgn :number sub [:number a :number b] (- a b))

(defn random-function
  "returns a random function with type typ"
  [typ]
  (case typ
    :number (rand-nth [add sub div mult])
    :otherwise (println "Damn")))

(defn random-terminal
  "returns a random terminal with type typ"
  [typ]
  (case typ
    :number (rand-nth '(x 0 1 2 3 4 5 6 7 8 9))
    :otherwise (println "Damn")))

(defn build-tree
  "Create a clojure program returning a value of type 'typ' with a maximum depth
  By default, the depth is 5"
  ([typ] (build-tree typ 5))
  ([typ depth]
  (if (or (= depth 0)
          (< (rand) 0.5)) (random-terminal typ)
  (let [func (random-function typ)]
    (cons func (map #(build-tree % (dec depth)) (:takes (meta func))))))))

(defn build-function [tree]
  "Returns a clojure callable function out of a clojure program (tree)"
  (eval (list 'fn '[x] tree)))

(defn replace-function
  "Takes an element e. If it's a function, returns its name, otherwise returns e"
  [e] (if (fn? e) (symbol (:nm (meta e))) e))

(defn pretty-tree [tree]
  "Print the clojure tree with function names instead of addresses"
  (w/postwalk replace-function tree))

(defn -main
  []
    (let [tree (build-tree :number)
          function (build-function tree)]
          (println (pretty-tree tree))
          ))
