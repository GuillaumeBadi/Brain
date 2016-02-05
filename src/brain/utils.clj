(ns brain.utils)

(defn- replace-function
  [e] (if (fn? e) (:name (meta e)) e))

(defn pretty-tree
  [tree]
  (if (seq? tree)
      (cons (replace-function (first tree))
            (map pretty-tree (rest tree)))
      tree))
