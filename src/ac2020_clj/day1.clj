(ns ac2020-clj.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


#_(defn spy
  ([v]
   (println v)
   v)
  ([s v]
   (println (str s v))
   v))


(defn file-as-val-s
  [filename]
  (->> (-> (io/resource filename)
           slurp
           (string/split #"\n"))
       (map #(Integer/parseInt %))))

#_(file-as-val-s "day1/input_sample.txt")
#_=> '(1721 979 366 299 675 1456)


(defn two-sum
  [sum entries]
  (let [entry-set (into #{} entries)]
    (->> entries
         (filter (fn [entry] (contains? (disj entry-set entry)
                                        (- sum entry))))
         (map (fn [entry] (set [entry
                                (- sum entry)])))
         distinct)))

(two-sum 10 [1 2 4 5 6 9 10 11 12])
#_=> '(#{1 9} #{4 6})


(do (def day1-answer-1
      (->> "day1/input.txt"
           file-as-val-s
           (two-sum 2020)
           first
           (reduce *)))
    day1-answer-1)
#_=> 788739

;--------------------------------------------------

(defn entry-sums
  [entries]
  (->> (into #{}
             (for [x (set entries)
                   y (disj (set entries) x)]
               #{x y}))
       (map (fn [entry-set] {(reduce + entry-set)
                             entry-set}))
       (apply merge)))

#_(entry-sums [1 2 3])
#_=> {4 #{1 3}, 3 #{1 2}, 5 #{3 2}}


(defn three-sum
  [sum entries]
  (let [sum->entry-set (entry-sums entries)
        my-fn          (fn [entry]
                         (let [complement (- sum entry)]
                           (when (contains? sum->entry-set complement)
                             (conj (get sum->entry-set complement)
                                   entry))))]
    (->> entries
         (map my-fn)
         (filter #(not (nil? %)))
         distinct)))
#_(three-sum 2020 [1721 979 366 299 675 1456])
#_=> => (#{979 366 675})


(do (def day1-answer-2
      (->> "day1/input.txt"
           file-as-val-s
           (three-sum 2020)
           first
           (reduce *)))
    day1-answer-2)
#_=> 178724430

