(ns tiny-maze.solver)

(defn- find-position
  [keyword maze]
  (loop [maze maze row (first maze) row-n 0 col-n 0]
    (if (seq maze)
      (if (seq row)
        (if (= keyword (first row))
          [row-n col-n]
          (recur maze (rest row) row-n (inc col-n)))
        (recur (rest maze) (second maze) (inc row-n) 0))
      [-1 -1])))

(def find-start (partial find-position :S))

(def find-end (partial find-position :E))

(defn- next-positions
  [pos]
  (map #(map + pos %) [[-1 0] [1 0] [0 -1] [0 1]]))

(defn- path?
  [maze pos]
  (let [v (get-in maze pos)]
    (and v (not= 1 v))))

(defn- filter-possible-directions
  [maze next-positions]
  (filter (partial path? maze) next-positions))

(defn- draw-path
  [maze path]
  (let [x (fn [_] :x)]
    (loop [path path solution maze]
      (if (seq path)
        (recur (rest path) (update-in solution (first path) x))
        solution))))

(defn- find-solutions
  [maze end-pos pos path visited]
  (when-not (contains? visited pos)
    (let [next-pos (filter-possible-directions maze (next-positions pos))]
      (if-not (some #{end-pos} next-pos)
        (mapcat #(find-solutions maze end-pos % (cons pos path) (conj visited pos)) next-pos)
        [(cons pos path)]))))

(defn solve-maze
  [maze]
  (let [start-pos (find-start maze)
        end-pos (find-end maze)
        solutions (find-solutions maze end-pos start-pos [] #{})]
    (when (seq solutions)
      (->> solutions
           (sort-by count)
           first
           (cons end-pos)
           (draw-path maze)))))
