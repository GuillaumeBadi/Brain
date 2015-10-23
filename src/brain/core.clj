(ns brain.core
  (require [clojure.walk :as w]
           [clojure.zip :as z]))

(def types #{:number})

(defn random-number []
  (if (< (rand) 0.5)
    'x
    (rand-nth '(1 2 3 4 5 6 7 8 9 0 10 11 12 13 14 15 16 17))))

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

(defn abs [n] (max n (- n)))

;; Some typed functions for the algorithm
(defgn :number add [:number a :number b] (+ a b))
(defgn :number mult [:number a :number b] (* a b))
(defgn :number div [:number a :number b] (if (zero? b) 0 (/ a b)))
(defgn :number _mod [:number a :number b] (if (zero? b) 0 (mod a b)))
(defgn :number sub [:number a :number b] (- a b))
; (defgn :number absolute [:number a] (abs a))
; (defgn :number sin [:number a] (Math/sin a))
; (defgn :number cos [:number a] (Math/cos a))
(defgn :number _if [:number a :number b :number c] (if (not (zero? a)) b c))

(def fns #{add sub div mult _mod _if})

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
    :number (random-number)
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
  (rand-nth (filter #(and
                      (not (nil? (z/node %)))
                      (returns-type typ %))
                    (locs tree))))

(def t (build-tree :number))
(pretty-tree t)

(pretty-tree (locs t))
(pretty-tree (filter #(and
          (not (nil? (z/node %)))
          (returns-type :number %))
        (locs t)))

(defn get-type
  [loc]
  (cond
    (number? (z/node loc)) :number
    (seq? (z/node loc)) (:typ (meta (first (z/node loc))))
     :else :number))

(defn mutate [tree]
  (let [loc (rand-nth (locs tree))]
    (z/root (z/replace loc (build-tree (get-type loc))))))

(def testree (build-tree :number))
(pretty-tree testree)
(pretty-tree (mutate testree))

(defn crossover [L R]
  (let [l (rand-nth (locs L))
        r (rand-nth (locs R))]
    [(replace-loc l r) (replace-loc r l)]))

;; (def ideal [[1 2] [2 4] [4 8] [8 16]])

; (defn ideal-function
;   [x]
;   (+ x (* x x) 1))

(defn ideal-function
  [x]
  (if (zero? (mod x 17))
  1
  0))

(def ideal
  (into [] (map #(vector % (ideal-function %))
       (range 0 50))))

(defn error
  [tree]
  (let [function (build-function tree)]
    (reduce + (map (fn [[input expected]]
                     (abs
                      (-
                       (function input) expected)))
                   ideal))))

(defn sort-by-error
  [population]
  (vec (map second
            (sort (fn [[err1 _] [err2 _]] (< err1 err2))
                  (map #(vector (error %) %) population)))))

(defn evolve
  [popsize typ]
  (loop [generation 0
         population (sort-by-error (repeatedly popsize #(build-tree typ)))]))

(def population (repeatedly 100 #(build-tree :number)))
(def pretty (map pretty-tree population))


(defn select
  [population tournament-size]
  (let [size (count population)]
    (nth population
         (apply min (repeatedly tournament-size #(rand-int size))))))

(defn evolve
  [popsize typ]
  (println "Starting evolution...")
  (loop [generation 0
         population (sort-by-error (repeatedly popsize #(build-tree typ)))]
    (let [best (first population)
          best-error (error best)]
      (println "Generation:" generation)
      (println "Best error:" best-error)
      (println "Best program:" (pretty-tree best))
      (println)
      (if (< best-error 0.1)
        (println "Success:" (pretty-tree best))
        (recur
          (inc generation)
          (sort-by-error
            (concat
             (repeatedly (* 1/2 popsize) #(mutate (select population 7)))
             (apply concat (repeatedly (* 1/4 popsize) #(crossover (select population 7)
                                                 (select population 7))))
             (repeatedly (* 1/4 popsize) #(select population 7)))))))))

(defn -main
  []
  (evolve 500 :number))


;;      pretty (map pretty-tree population)]
;;        pretty))
