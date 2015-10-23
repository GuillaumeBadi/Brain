

(defn abs [n] (max n (- n)))

(def sub -)
(def add +)
(def mult *)
(def div /)
(def absolute abs)
(def _mod mod)

(defn _if [a b c] (if (not (zero? a)) b c))

(def x 17)

(_if (_mod x 17) (div 13 (_mod (_if (mult (mult x (_mod 17 4)) (_mod 0 9)) x 2) (mult (sub 2 2) x))) (_if 7 (add 1 (div (_if (sub 3 (_mod 11 x)) (_if (_mod x (mult (sub 2 2) x)) 5 1) 11) x)) (div (div (_if 1 x x) x) x)))