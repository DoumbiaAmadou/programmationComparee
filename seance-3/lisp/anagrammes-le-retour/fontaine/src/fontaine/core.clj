(ns fontaine.core
  (:require [clojure.java.io :as io]))

(defn get-words
  "Return a non-lazy sequence of all words in the given file"
  [filename]
  (with-open [r (io/reader filename)]
    (doall (line-seq r))))

(defn mk-key
  [word]
  (apply str (sort word)))

(defn mk-dict
  "Return a map of <sorted string> => list of words"
  [words]
  (loop [dict {}
         [word & words] words]
    (if words
      (let [k (mk-key word)]
        (recur
          (assoc dict k (cons word (dict k)))
          words))
      dict)))

(defn -main
  [& args]
  (let [words (get-words "../words")
        dict (mk-dict words)]
    (doseq [word args]
      (do
        (printf "%s: " word)
        (doseq [anag (dict (mk-key word))]
          (printf "%s " anag))
        (println)))))
