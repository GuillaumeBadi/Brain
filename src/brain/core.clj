(ns brain.core
  (require  [clojure.pprint :as pp]
            [clojure.walk :as w]))

(defn get-types
  [v] (into [] (filter keyword? v)))

(defn get-values
  [v] (into [] (filter (complement keyword?) v)))

(defmacro defgn
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

(defgn :number add [:number a :number b] (+ a b))
(defgn :number mult [:number a :number b] (* a b))
(defgn :number div [:number a :number b] (if (zero? b) 0 (/ a b)))
(defgn :number sub [:number a :number b] (- a b))

(defn random-function
  [typ]
  (case typ
    :number (rand-nth [add sub div mult])
    :otherwise (println "Damn")))

(defn random-terminal
  [typ]
  (case typ
    :number (rand-nth '(x 0 1 2 3 4 5 6 7 8 9))
    :otherwise (println "Damn")))

;; Create a Clojure tree
(defn build-tree
  ([typ] (build-tree typ 5))
  ([typ depth]
  (if (or (= depth 0)
          (< (rand) 0.5)) (random-terminal typ)
  (let [func (random-function typ)]
    (cons func (map #(build-tree % (dec depth)) (:takes (meta func))))))))

(defn build-function [tree] (eval (list 'fn '[x] tree)))

(defn replace-function
  [e] (if (fn? e) (symbol (:nm (meta e))) e))

(defn pretty-tree [tree]
  (w/postwalk replace-function tree))

(defn -main
  []
    (let [tree (build-tree :number)
          function (build-function tree)]
          (println (pretty-tree tree))
          ))
