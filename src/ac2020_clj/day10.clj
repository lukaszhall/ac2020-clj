(ns ac2020-clj.day10
  (:require [ac2020-clj.util :as util]
            [clojure.string :as string]))

;;sample input_sample
(util/file-as-seq "day10/input_sample.txt")
#_=> ["16" "10" "15" "5" "1" "11" "7" "19" "6" "12" "4"]


(defn arranged-adapters
  "Sort and bookend (Add starting [0] and ending [max+3] joltages) to an input set"
  [adapters]
  (let [adapters-with-ends (conj adapters 0 (+ 3 (apply max adapters)))
        sorted             (-> adapters-with-ends
                               #_distinct
                               sort)]
    (into [] sorted)))

(defn joltages
  "Computer joltages in adapter chain"
  [adapters]
  (->> (partition 2 1 adapters)
       (map (fn [[a b]] (- b a)))
       (apply vector)))


;----- utility

(defn drop-nth-v
  "remove nth elem in vector"
  [n v]
  (into (subvec v 0 n) (subvec v (inc n))))

(defn drop-nth
  "remove nth elem in any coll"
  [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

(defn rest-v
  "vector without first element"
  [v]
  (subvec v 1 (count v)))

;--------- part 2

(defn valid-chain?
  "All joltages in an adapter chain must be <= 3. Does not check boundary conditions (start 0, etc..)"
  [adapters]
  (->> adapters
       joltages
       (not-any? #(> % 3))))



(def variations-count
  "Number of permuations of the given adapters that have valid (3 jolts max) chains from end to end"
  (memoize
    (fn
      [adapters]
      (if (<= (count adapters) 2)
        1
        (let [dropped-1st (drop-nth-v 1 adapters)
              ]
          (if (not (valid-chain? dropped-1st))
            (variations-count (rest-v adapters))
            (+ (variations-count (rest-v adapters))
               (variations-count dropped-1st))))))))

;; Visualization variant
#_(def variations-count
    "Number of permuations of the given adapters that have valid (3 jolts max) chains from end to end"
    (memoize
      (fn
        [adapters]
        (if (<= (count adapters) 2)
          1
          (let [dropped-1st (drop-nth-v 1 adapters)
                ]
            (if (not (valid-chain? dropped-1st))
              (do
                (let [counts (variations-count (rest-v adapters))]
                  (print (str "adapters: " adapters "  "))
                  (print "variations count for rest-v " (rest-v adapters))
                  (println (str "  counts:->" counts))
                  counts))
              (do
                (let [whole-count   (variations-count (rest-v adapters))
                      dropped-count (variations-count dropped-1st)]
                  (print (str "adapters: " adapters "  "))
                  (print "variations count for rest-v and dropped 1-st" (rest-v adapters) "  " (rest-v dropped-1st))
                  (println (str "  whole/dropped counts:-> " whole-count "  " dropped-count))
                  (+ whole-count dropped-count)))))))))


(defn variations
  "return all valid permuations of an adapter chain"
  [adapters]
  (loop [idx          (- (count adapters) 2)
         valid-chains (conj '() adapters)
         ]
    (println valid-chains)
    (if (<= idx 0)
      valid-chains
      (let [valid-dropped-nth-chains (->> (mapv #(drop-nth-v idx %) valid-chains)
                                          (filterv valid-chain?))
            all-chains               (concat valid-chains valid-dropped-nth-chains)]
        (println "-----------------------------")
        (println (str "index: " idx))
        (println (str "New Valid chains: " valid-dropped-nth-chains))
        (println (str "All chains: " (into [] all-chains)))

        (recur (dec idx)
               all-chains)))))


(comment

  (do (def sample-input (->> (util/file-as-seq "day10/input_sample.txt")
                             (map #(Integer/parseInt %))))
      sample-input)
  #_=> '(16 10 15 5 1 11 7 19 6 12 4)

  (do (def sample-input2 (->> (util/file-as-seq "day10/input_sample2.txt")
                              (map #(Integer/parseInt %))))
      sample-input2)
  #_=> '(28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3)

  (do (def full-input (->> (util/file-as-seq "day10/input.txt")
                           (map #(Integer/parseInt %))))
      full-input)
  #_=> '(104 83 142 123,,,,,,)


  (->> sample-input
       arranged-adapters)
  #_=> [0 1 4 5 6 7 10 11 12 15 16 19 22]


  (joltages [0 1 4 5 6 7 10 11 12 15 16 19 22])
  #_=> [1 3 1 1 1 3 1 1 3 1 3 3]


  ;; Part 1
  (->> full-input
       arranged-adapters
       joltages
       frequencies
       (#(* (get % 1)
            (get % 3))))
  #_=> 2201

  ;-- part 2

  {:valid-3inc     (valid-chain? [0 1 4 7 10])
   :invalid-4inc   (valid-chain? [0 1 4 8 10])
   :valid-mixinc   (valid-chain? [0 1 2 3 4 7 10])
   :invalid-mixinc (valid-chain? [0 1 2 3 4 7 10 14])}
  #_=> {:valid-3inc true, :invalid-4inc false, :valid-mixinc true, :invalid-mixinc false}


  [(drop-nth-v 0 [0 1 2 3 4 5])
   (drop-nth-v 1 [0 1 2 3 4 5])
   (drop-nth-v 4 [0 1 2 3 4 5])
   (drop-nth-v 5 [0 1 2 3 4 5])]
  #_=> [[1 2 3 4 5] [0 2 3 4 5] [0 1 2 3 5] [0 1 2 3 4]]

  [(drop-nth 0 [0 1 2 3 4 5])
   (drop-nth 1 [0 1 2 3 4 5])
   (drop-nth 4 [0 1 2 3 4 5])
   (drop-nth 5 [0 1 2 3 4 5])]
  #_=> ['(1 2 3 4 5) '(0 2 3 4 5) '(0 1 2 3 5) '(0 1 2 3 4)]


  (rest-v [0 1 2 3 4])
  #_=> [1 2 3 4]

  (variations-count [0 1 4 5 6 8 11])
  #_=> 3

  (variations-count (->> sample-input
                         arranged-adapters))
  #_=> 8

  (variations-count (->> sample-input2
                         arranged-adapters))
  #_=> 19208

  (variations-count (->> full-input
                         arranged-adapters))
  #_=> 169255295254528


  (variations [0 1 4 5 6 8 11])
  #_=> '([0 1 4 5 6 8 11] [0 1 4 5 8 11] [0 1 4 6 8 11])


  (variations (->> sample-input
                   arranged-adapters))
  #_=> '([0 1 4 5 6 7 10 11 12 15 16 19 22]
         [0 1 4 5 6 7 10 12 15 16 19 22]
         [0 1 4 5 7 10 11 12 15 16 19 22]
         [0 1 4 5 7 10 12 15 16 19 22]
         [0 1 4 6 7 10 11 12 15 16 19 22]
         [0 1 4 6 7 10 12 15 16 19 22]
         [0 1 4 7 10 11 12 15 16 19 22]
         [0 1 4 7 10 12 15 16 19 22])

  )
