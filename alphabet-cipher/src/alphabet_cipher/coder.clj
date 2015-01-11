(ns alphabet-cipher.coder)

(def alphabet (cycle "abcdefghijklmnopqrstuvwxyz"))
(def alphabet-length 26)

(defn- index [c] (- (int c) (int \a)))

(defn- find-row [k] (drop (index k) alphabet))

(defn- encode-char
  "Encode the char c with key k"
  [c k]
  (nth (find-row k) (index c)))

(defn- decode-char
  "Decode the encoded char c with key k"
  [c k]
  (let [row-first-char-index (index (first (find-row k)))
        c-index (+ (- (index c) row-first-char-index) alphabet-length)]
    (nth alphabet c-index)))

(defn- transcode [f keyword message]
  (apply str (map f message (cycle keyword))))

(defn encode [keyword message]
  (transcode encode-char keyword message))

(defn decode [keyword encoded-message]
  (transcode decode-char keyword encoded-message))
