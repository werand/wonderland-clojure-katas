(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

;; To find doublets first the number of possible words is limited by
;; filtering the words that have the correct length.
(defn- filter-words-by-length
  "A set of words with length n based on words."
  [words n]
  (set (filter #(= (.length %) n) words)))

;; Calculate the the difference of words which is the number of different
;; characters.
(defn- char-diff
  "The char-diff returns 0 if the characters are equal and 1 if characters are not equal."
  [c1 c2]
  (if (= c1 c2) 0 1))

;; With char-diff it is easy to calculate the number of different characters
(defn- word-diff
  "Distance of word1 and word2."
  [word1 word2]
  (reduce + (map char-diff word1 word2)))

;; To find the next word from a list of words, the words are filtered to just
;; contain words which have only one different character
(defn- filter-words-with-diff-1
  [word words]
  (filter #(= 1 (word-diff word %)) words))

(defn leaves
  "Given a tree of solutions, return the solutions as a list."
  [coll]
  (when-let [s (seq coll)]
    (if (some sequential? s)
      (mapcat leaves coll)
      (list coll))))

(defn doublets
  ;; The possible-next-words are used to search the paths from word1 to word2 to
  ;; build up the result. Since more than one word may be found with only
  ;; one difference all possible paths are calucated - resulting in a tree
  ;; of solutions.
  ([result possible-next-words word1 word2]
     (let [words-with-diff-1 (filter-words-with-diff-1 word1 possible-next-words)]
       (if (seq words-with-diff-1)
         (map #(if-not (= % word2)
                    (doublets (conj result %) (disj possible-next-words %) % word2)
                    (conj result word2))
              words-with-diff-1)
         [])))
  ;; Find the doublets by first checking that word1 and word2 have equal
  ;; length. If equal in length the possible words are looked up by filtering
  ;; the words - using only words with correct length for further computation.
  ;; All word paths are computed, the longest path is returned.
  ([word1 word2]
     (if (= (count word1) (count word2))
       (let [possible-next-words (disj (filter-words-by-length words (count word1)) word1)
             doublet-result-tree (doublets [word1] possible-next-words word1 word2)]
         (first (sort-by count > (leaves doublet-result-tree))))
       [])))
