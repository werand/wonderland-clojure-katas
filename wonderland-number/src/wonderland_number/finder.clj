(ns wonderland-number.finder)

(def digits [1 2 3 4 5 6 7 8 9 0])

(defn n-of-k
  ([n s] (n-of-k n s []))
  ([n s acc]
     (if (> n 0)
       (when (seq s)
         (mapcat (fn [x] (n-of-k (dec n) (remove #(= x %) s) (cons x acc))) s))
       [acc])))

(defn to-seq
  [n]
  (loop [x 100000
         n n
         r []
         count 0]
    (let [c (int (/ n x))]
      (if (< count 6)
        (recur (/ x 10) (- n (* c x)) (conj r c) (inc count))
        r))))

(defn to-number
  [s]
  (reduce + (map #(* %1 %2) [1 10 100 1000 10000 100000] s)))

(defn seq-multiplied-contains-same-digits?
  [n s]
  (->> s
       to-number
       (* n)
       to-seq
       set
       (= (set s))))

(defn wonderland-number []
  (to-number
   (first
    (->>
     digits
     (n-of-k 6)
     (filter (partial seq-multiplied-contains-same-digits? 2))
     (filter (partial seq-multiplied-contains-same-digits? 3))
     (filter (partial seq-multiplied-contains-same-digits? 4))
     (filter (partial seq-multiplied-contains-same-digits? 5))
     (filter (partial seq-multiplied-contains-same-digits? 6))))))
