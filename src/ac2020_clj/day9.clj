(ns ac2020-clj.day9
  (:require [ac2020-clj.util :as util]
            [clojure.string :as string]))

;;sample input_sample
(util/file-as-seq "day9/input_sample.txt")
#_=> ["35" "20" "15" "25" "47" "40" "62" "55" "65" "95" "102" "117" "150" "182" "127" "219" "299" "277" "309" "576"]

(defn sum-pair
  "Return pair of integers from preample which add to sum, or nil"
  [preamble sum]
  (-> (for [n1 preamble
            n2 preamble
            :when (and (> n2 n1)
                       (= sum (+ n1 n2)))]
        [n1 n2])
      first))

(defn invalid-number-s
  "Return lazy seq of all invalid numbers, where a number is invalid if
  no numbers preceeding it (up to 'size') sum to that number"
  [size num-s]
  (for [window (partition (inc size) 1 num-s)
        :let [preamble (butlast window)
              sum      (last window)]
        :when (not (sum-pair preamble sum))]
    sum))

;; Part 2

(defn summing-range-matching-first-invalid-number
  [size num-s]
  (let [invalid-number (first (invalid-number-s size num-s))
        num-v          (vec num-s)]
    (loop [sum-start-idx 0
           sum-end-idx   0
           running-sum   0]
      ;(println {:sum running-sum :start sum-start-idx :end sum-end-idx :diff (- invalid-number running-sum)})
      (cond
        (= invalid-number running-sum) (subvec num-v sum-start-idx sum-end-idx)

        (< running-sum invalid-number) (recur sum-start-idx
                                              (inc sum-end-idx)
                                              (+ running-sum (nth num-v sum-end-idx)))

        (< invalid-number running-sum) (recur (inc sum-start-idx)
                                              sum-end-idx
                                              (- running-sum (nth num-v sum-start-idx)))))))


(comment

  (sum-pair [1 2 3 4 5] 7)
  #_=> [2 5]

  (sum-pair [1 2 3 4 5] 27)
  #_=> nil

  (partition (inc 2) 1 [1 2 3 4 5 6 7])
  #_=> '((1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6 7))

  (invalid-number-s 5 [1 2 3 4 9 10 50 60 62 63])
  #_=> '(50 62 63)

  ;; Part 1
  (-> (->> (util/file-as-seq "day9/input.txt")
           (map #(BigInteger. %))
           (invalid-number-s 25))
      first)
  #_=> 258585477

  (->> (util/file-as-seq "day9/input.txt")
       (map #(BigInteger. %))
       (summing-range-matching-first-invalid-number 25))
  #_=> [13858643
        9455395
        9908827
        16794010
        13221299
        11563238
        12646458
        11137204
        11774548
        12220424
        14302571
        14304519
        14748447
        25865809
        22680253
        16578014
        27525818]


  ;; Part 2
  (-> (->> (util/file-as-seq "day9/input.txt")
           (map #(BigInteger. %))
           (summing-range-matching-first-invalid-number 25))

      (#(+ (apply max %)
           (apply min %))))
  #_=> 36981213N

  )
