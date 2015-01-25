(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :as s]))

;; The rules for this puzzle are:
;;
;; You must get the fox, goose, and bag of corn safely across the other side of the river
;; You can only carry 1 item on the boat across with you.
;; The fox cannot be left alone with the goose, (or it will be eaten).
;; The goose cannot be left alone with the corn, (or it will be eaten).
;;
;; The goal is to have the plan in steps so that all make it safely to the other side
;; [[[:fox :goose :corn :you] [:boat] []]
;; ...
;; [[[] [:boat] [:fox :goose :corn :you]]]]

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn- remove-candidates
  [place candidate]
  (if (seq candidate)
    (remove-candidates (remove #(= (first candidate) %) place) (rest candidate))
    place))

(defn- next-candidates
  [place]
  (let [place (remove #(= :you %) place)]
    (into [[:you]] (map #(vector :you %) place))))

(defn- safe?
  "Determines if it is safe on the place."
  [place]
  (if (some #{:you} place)
    true
    (let [place (set place)]
      (< (+ (count (s/intersection #{:fox :goose} place))
            (count (s/intersection #{:goose :corn} place)))
         3))))

(declare carry-back)

(defn- carry-across
  [[here boat there] path known]
  (when (safe? here)
    (let [path (cons [here boat there] path)
          new-there (remove-candidates (concat there boat) [:boat])
          path (cons [here [:boat] new-there] path)]
      (if (= (set new-there) #{:fox :goose :corn :you})
        [path]
        (let [next-candidates (next-candidates new-there)]
          (mapcat #(carry-back [here (concat [:boat] %) (remove-candidates new-there %)] path known) next-candidates))))))

(defn- carry-back
  [[here boat there] path known]
  (when (and (safe? there)
             ((complement contains?) known here))
    (let [path (cons [here boat there] path)
          new-here (remove-candidates (concat here boat) [:boat])
          path (cons [new-here [:boat] there] path)]
      (let [next-candidates (next-candidates new-here)]
        (mapcat #(carry-across [(remove-candidates new-here %) (concat [:boat] %) there] path (conj known here)) next-candidates)))))

(defn- find-solution
  [[here boat there]]
  (carry-back [here boat there] [] #{}))

(defn river-crossing-plan []
  (->> start-pos
       first
       find-solution
       first
       reverse))
