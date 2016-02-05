(ns brain.utils)

(defn abs [n] (max n (- n)))

(defn- replace-function
  [e] (if (fn? e) (:name (meta e)) e))

(defn score [got expected]
  "| expected - got | * -1"
  (* -1 (abs (- expected got))))

(defn pretty-tree
  [tree]
  (if (seq? tree)
      (cons (replace-function (first tree))
            (map pretty-tree (rest tree)))
      tree))
