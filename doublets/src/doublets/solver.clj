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

;; Find the next word the words are filtered to just contain words which
;; have only one different character
(defn- filter-words-with-diff-1
  [word words]
  (filter #(= 1 (word-diff word %)) words))

(defn doublets
  ;; Find the doublets by first checking that the passed words have equal
  ;; length. If equal the possible words are looked up by filtering the words
  ;; with correct length and only one difference to limit the search space.
  ([word1 word2]
     (if (= (count word1) (count word2))
       (let [possible-words (filter-words-by-length words (count word1))
             possible-words (disj possible-words word1)]
         (doublets [word1] possible-words word1 word2))
       []))
  ;; These possible-words are used to search the path from word1 to word2 and
  ;; build up the result. Since more than one word may be found with only
  ;; one difference all paths are mapcat'ed together by recursively calling
  ;; the function again, conj the new word to the result and reducing the
  ;; search space.
  ([result possible-words word1 word2]
     (let [found-words (filter-words-with-diff-1 word1 possible-words)]
       (if (seq found-words)
         (mapcat #(if-not (= % word2)
                    (doublets (conj result %) (disj possible-words %) % word2)
                    (conj result word2))
                 found-words)
         []))))
