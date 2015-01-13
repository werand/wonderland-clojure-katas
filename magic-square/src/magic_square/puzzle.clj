(ns magic-square.puzzle)

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn- permutations
  [s]
  (if (seq s)
    (if (= 2 (count s))
      [[(first s) (second s)] [(second s) (first s)]]
      (mapcat (fn [x]
                (map (fn [y] (concat [x] y))
                     (permutations (filter #(not= x %) s))))
              s))
    s))

(defn- sum-rows [m]
  (map #(reduce + %) m))

(defn- sum-cols [m]
  [(reduce + (map first m))
   (reduce + (map second m))
   (reduce + (map last m))])

(defn- sum-diagonals [m]
  [(+ (get-in m [0 0]) (get-in m [1 1]) (get-in m [2 2]))
   (+ (get-in m [2 0]) (get-in m [1 1]) (get-in m [0 2]))])

(defn- solution?
  [m]
  (= (set (sum-rows m))
     (set (sum-cols m))
     (set (sum-diagonals m))))

(defn- find-solution
  "Brute force search of all solutions to the magic square puzzle."
  [values]
  (let [p (map #(vec (map vec (partition 3 %))) (permutations values))]
    (filter solution? p)))

(defn magic-square
  "Return the first solution"
  [values]
  (first (find-solution values)))

;; The first solution is [[1.0 1.5 3.5] [4.5 2.0 4.0] [5.0 2.5 3.0]]
;; but there are 55 more ...
