(ns ac2020-clj.testing)


(defn test-data
  [[min-n max-n]]
  (->> (range min-n max-n)
       (map (fn [n] (Long. n)))
       (map (fn [n] (String/format "%05d" (into-array [n]))))))

(defn format-stores
  [stores-per-line stores]
  (->> (partition stores-per-line stores)
       (map (fn [numbers] (->> (map #(str "\"" % "\" ") numbers)
                               (clojure.string/join))))
       (clojure.string/join "\n")))

(->> (test-data [10 400])
     (format-stores 10)
     (println))

