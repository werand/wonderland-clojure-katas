(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn- filter-words-by-length
  [n]
  (set (filter #(= (count %) n) words)))

(defn- word-diff [word1 word2]
  (apply + (map #(if (= (int %1) (int %2)) 0 1) word1 word2)))

(defn- find-words-with-diff-1
  [word words]
  (filter #(= 1 (word-diff word %)) words))

(defn doublets
  ([word1 word2]
     (if (= (count word1) (count word2))
       (let [possible-words (filter-words-by-length (count word1))
             possible-words (disj possible-words word1)]
         (doublets [word1] possible-words word1 word2))
       []))
  ([result possible-words word1 word2]
     (let [found-words (find-words-with-diff-1 word1 possible-words)]
       (if (seq found-words)
         (mapcat #(if-not (= % word2)
                    (doublets (conj result %) (disj possible-words %) % word2)
                    (conj result word2)) found-words)
         []))))
