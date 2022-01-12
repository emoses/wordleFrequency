(ns matches)

(defn n-matches [w target]
  (reduce + (map (fn [a b] (if (= a b) 1 0)) w target)))

(defn count-matches [w]
  (partial n-matches w))

(defn add-seq [a b]
  (map + a b))

(defn do-calc [all-words target-words]
  (let [counter (apply juxt (map count-matches target-words))]
    (reduce (fn [acc w] (if-not (= 5 (count w)) acc (doall (map + acc (counter w))))) (map (constantly 0) target-words) all-words)))


(defn -main [dict-file & words]
  (with-open [rdr (clojure.java.io/reader dict-file)]
    (let [dict (line-seq rdr)]
      (print (map vector words (do-calc dict words))))))
