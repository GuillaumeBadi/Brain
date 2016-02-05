(ns brain.types)

(comment
  "For each type, we need a map:
   {:random-function :random-terminal :extends}")

(defn- combine
  "Choose a random function in v and returns its result"
  [v]
  (fn [] (rand-nth (map #(%) v))))

(defn datatype
  "Defines a datatype
   [name random-function random-terminal]:
      create a raw datatype
   [name extends]:
      create a higher level datatype
   exemple: (def number (datatype :number [float integer]))"
  ([name random-terminal random-function]
   {:name name
    :random-terminal random-terminal
    :random-function random-function})
  ([name extends]
   {:name name
    :random-terminal (combine (map #(:random-terminal %) extends))
    :random-function (combine (map #(:random-function %) extends))}))

(defn not-nil? [e] (not (nil? e)))

(defn get-values
  [v]
  [(filter not-nil? (map-indexed #(if (zero? (mod %1 2)) %2 nil) v))
   (filter not-nil? (map-indexed #(if (zero? (mod %1 2)) nil %2) v))])


(defn with-parameters
  [return-type params]
  (let [args (->> params
                  (partition 2)
                  (filter #(= return-type (first %)))
                  (map #(fn [] (second %))))]
    (combine (cons (:random-terminal return-type) args))))

(defmacro defgn
  "Macro used to decalre typed functions"
  [t n args & body]
  (let [[types values] (get-values args)]
     `(def ~n
         ^{:name ~(name n)
           :type ~t
           :takes ~(into [] types)}
         (fn ~n ~(into [] values) ~@body))))
