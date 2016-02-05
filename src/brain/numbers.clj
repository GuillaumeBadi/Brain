(ns brain.numbers
  (require [brain.types :as types]))

(def add)
(def sub)
(def mult)
(def div)
(def modulo)

(def gen-integer
  (types/datatype :integer
                  #(bigint (rand-int 50))
                  #(rand-nth [add sub mult])))

(def gen-float
  (types/datatype :float
                  #(bigint (rand-int 50))
                  #(rand-nth [add sub mult])))

(def gen-number
  (types/datatype :number
                  [gen-float gen-integer]))

(types/defgn gen-number
    add [gen-number a gen-number b]
        (apply + [a b]))

(types/defgn gen-number
    sub [gen-number a gen-number b]
        (- a b))

(types/defgn gen-number
    div [gen-number a gen-number b]
        (if (= 0 b) 0 (apply / [a b])))

(types/defgn gen-number
    mult [gen-number a gen-number b]
         (apply * [a b]))

(types/defgn gen-number
   modulo [gen-number a gen-number b]
          (if (= b 0) 0 (mod a b)))
