(ns main)

(defn n-length [n]
  (fn [word]
    (= (count word) n)))

(def lower-chars (map char (range 97 123)))

(defn empty-letter-map []
  (into {} (map (juxt identity (constantly 0)) lower-chars)))

(defn count-freq [m chars]
  (reduce (fn [m c] (update m c inc)) m chars))

(defn do-count [all-words]
  (let [res (empty-letter-map)
        xf (comp
            (filter (n-length 5))
            (map distinct))]
    (transduce xf (completing count-freq) (empty-letter-map) all-words)))

(defn print-result [m]
  (let [freqs (->> m
                   (into [])
                   (sort (fn [[_ a] [_ b]] (compare b a))))]
    (doall (map (fn [[c n]] (print (format "%s: %d\n" c n))) freqs))
    (print (concat (keys freqs)))
    (print "\n")))

(defn -main [& args]
  (print "hi\n")
  (with-open [rdr (clojure.java.io/reader (first args))]
    (-> (line-seq rdr)
        (do-count)
        (print-result))))
