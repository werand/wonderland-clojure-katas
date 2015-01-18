(ns wonderland-number.finder)

(def digits [1 2 3 4 5 6 7 8 9 0])
(def magic-number-digits-count 6)

(defn n-of-k
  "Creates a list of all possible ways to take n elements from a list s"
  ([n s] (n-of-k n [] s))
  ([n acc s]
     (if (> n 0)
       (when (seq s)
         (mapcat (fn [x] (n-of-k (dec n) (cons x acc) (remove #(= x %) s))) s))
       [acc])))

(def power-of-tens (cons 1 (iterate #(* 10 %) 10)))

(defn to-seq
  "Creates a list of size c of the digits of x."
  [c x]
  (map #(int (/ (mod x %) (/ % 10))) (reverse (take c (rest power-of-tens)))))

(defn to-number
  "Calculates the corresponding number for a sequence s."
  [s]
  (reduce + (map #(* %1 %2) power-of-tens s)))

(defn seq-multiplied-contains-same-digits?
  "Calculates a number for digits in s and tests that the result has the same digits."
  [n s]
  (->> s
       to-number
       (* n)
       (to-seq magic-number-digits-count)
       set
       (= (set s))))

(defn wonderland-number []
  (time (to-number
         (first
          (->>
           digits
           ;; The search space can be reduced by knowing that the higest power of 10 must be
           ;; the digit 1 since, (int (/ 1000000 6)) => 166666. It could be further optimized
           ;; but alone this optimization limits the search space by a factor of 10
           (remove #(= 1 %))
           (n-of-k (dec magic-number-digits-count) [1])
           (filter (partial seq-multiplied-contains-same-digits? 6))
           (filter (partial seq-multiplied-contains-same-digits? 5))
           (filter (partial seq-multiplied-contains-same-digits? 4))
           (filter (partial seq-multiplied-contains-same-digits? 3))
           (filter (partial seq-multiplied-contains-same-digits? 2)))))))
