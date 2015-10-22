(ns brain.core
  (require [clojure.walk :as w]
           [clojure.zip :as z]))

(def types #{:number})

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

(def fns #{add sub div mult})

(defn random-function
  "returns a random function with type typ"
  [typ]
  (case typ
    :number (rand-nth (into [] fns))
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

(defn functionize [tree]
  (list 'fn '[x] tree))

(defn build-function [tree]
  "Returns a clojure callable function out of a clojure program (tree)"
  (eval (functionize tree)))

(defn replace-function
  "Takes an element e. If it's a function, returns its name, otherwise returns e"
  [e] (if (fn? e) (symbol (:nm (meta e))) e))

(defn pretty-tree [tree]
  "Print the clojure tree with function names instead of addresses"
  (w/postwalk replace-function tree))

(defn serialize [tree]
  (functionize (pretty-tree tree)))

(defn save-tree [tree]
  (spit "save.clj" (serialize tree)))

(defn get-tree [file]
  (-> file slurp read-string))

(def fun (eval (get-tree "save.clj")))

(defn locs [tree]
  (let [zipper (z/seq-zip tree)
        all-locs (take-while (complement z/end?) (iterate z/next zipper))]
    (filter #(not (fns (z/node %))) all-locs)))

(defn replace-loc [l r]
  (z/root (z/replace l (z/node r))))

(defn check-type [typ e]
  (case typ
    :number ((fn [x] (number? x)) e)))

(defn returns-type [typ loc]
  (let [e (z/node loc)]
    (if (seq? e)
        (= (:typ (meta (first e))) typ)
      (if (fns e) false (check-type typ e)))))

(defn get-random-loc [tree typ]
  (rand-nth (filter #(returns-type typ %) (locs tree))))

(def tree (build-tree :number))
(def tree2 (build-tree :number))

(def function (build-function tree))
(pretty-tree tree)
(function 2)

(z/node (get-random-loc tree :number))

(defn mutate [tree]
  (let [t (rand-nth (into [] types))]
    (z/root (z/replace (get-random-loc tree t) tree))))

(pretty-tree (mutate tree))

(defn crossover [L R]
  (let [l (rand-nth (locs L))
        r (rand-nth (locs R))]
    [(replace-loc l r) (replace-loc r l)]))

(defn -main
  [])
