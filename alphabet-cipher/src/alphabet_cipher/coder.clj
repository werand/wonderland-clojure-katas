(ns alphabet-cipher.coder)

(def alphabet (cycle "abcdefghijklmnopqrstuvwxyz"))

(defn- index
  [c]
  (- (int c) (int \a)))

(defn- encode-char
  "Encode the char c with key k"
  [c k]
  (nth (drop (index k) alphabet) (index c)))

(defn encode [keyword message]
  (apply str (map encode-char message (cycle keyword))))

(defn- to-char [n]
  (char (+ (int \a) n)))

(defn- decode-char
  "Decode the encoded char c with key k"
  [c k]
  (let [row (drop (index k) alphabet)]
    (loop [index 0]
      (if (= c (nth row index))
        (to-char index)
        (recur (inc index))))))

(defn decode [keyword encoded-message]
  (apply str (map decode-char encoded-message (cycle keyword))))
