(ns wonderland-number.finder)

(defn n-von-k
  ([s n]
     (n-von-k s n []) )
  ([s n acc]
     (if (> n 0)
       (if (seq s)
         (map (fn [x]
                (n-von-k (remove #(= x %) s) (dec n) (cons x acc)))
              s))
       acc)))

(defn wonderland-number []
  ;; calculate me
  42)
