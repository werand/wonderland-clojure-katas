(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(def numeric-ranks {2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 :jack 10 :queen 11 :king 12 :ace 13
            :spade 1 :club 2 :diamond 3 :heart 4})

(defn numeric-rank
  [k]
  (get numeric-ranks k 0))

(defn play-round
  [player1-card player2-card]
  (let [[player1-suit-rank player1-rank] (map numeric-rank player1-card)
        [player2-suit-rank player2-rank] (map numeric-rank player2-card)]
    (cond
        (> player1-rank player2-rank) :player1
        (< player1-rank player2-rank) :player2
        :else (if (> player1-suit-rank player2-suit-rank)
                :player1
                :player2))))

(defn- vec-rest [v] (into [] (rest v)))

(defn play-game
  [player1-cards player2-cards]
  (loop [player1-cards player1-cards
         player2-cards player2-cards]
    (if (seq player1-cards)
      (if (seq player2-cards)
        (let [player1-card (first player1-cards)
              player2-card (first player2-cards)]
          (condp = (play-round player1-card player2-card)
            :player1 (recur (conj (vec-rest player1-cards) player1-card player2-card)
                            (vec-rest player2-cards))
            :player2 (recur (vec-rest player1-cards)
                            (conj (vec-rest player2-cards) player2-card player1-card))
            :unexpected))
        :player1-wins)
      :player2-wins)))
