;; types
;; numbers
;; utils
;; function
;; tree
;; evolution

(ns brain.tree
  (require [brain.utils :as utils]
           [brain.types :as types]
           [brain.numbers :as numbers]
           [brain.function :as function]))

(defn build-tree
  "Create a random typed Clojure program
   Of max `depth` depth and a given return type"
  ([parameters return-type] (build-tree parameters return-type 5))
  ([parameters return-type depth]
   (let [terminal (types/with-parameters return-type parameters)
         function (:random-function return-type)]
        (if (or (> (rand) 0.5)
                (= depth 0))
            (terminal)
            (let [func (function)]
                 (cons func
                       (map #(build-tree parameters % (dec depth))
                            (:takes (meta func)))))))))

(defn mutate
  "Insert a tree at a random index of the individual"
  [params individual base-type]
  (if (or (not (seq? individual))
          (< (rand) 0.5))
      ; then
      (build-tree params base-type)
      ; else
      (let [args (rest individual)
            index (rand-int (- (count args) 1))
            target-type (nth (:takes (meta (first individual))) index)]
          (cons (first individual)
                (map-indexed #(if (= %1 index)
                                  (mutate params %2 target-type)
                                  %2) args)))))

(defn make-population
  "Create a population of trees"
  [parameters size base-type depth]
  (repeatedly size #(build-tree parameters base-type depth)))
