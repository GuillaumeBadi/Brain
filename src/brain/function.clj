(ns brain.function
  (require [brain.utils :as utils]
           [brain.types :as types]))

(defn tree-to-function
  [tree params]
  (let [[types values] (types/get-values params)]
    (eval (list 'fn (into [] values) tree))))

; [numbers/gen-number 'x]
;
; (def tree (tree/build-tree [numbers/gen-number 'x] numbers/gen-number))
; (def params [numbers/gen-number 'x])
;
; (println (utils/pretty-tree tree))
; (tree-to-function tree [numbers/gen-number 'x])
