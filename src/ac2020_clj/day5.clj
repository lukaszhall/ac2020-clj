(ns ac2020-clj.day5
  (:require [ac2020-clj.util :as util]
            [clojure.string :as string]))

;; Sample inputs
(util/file-as-seq "day5/input_sample.txt")
#_=> ["BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL"]


(def ch->bit {\F 0
              \B 1
              \L 0
              \R 1})


(defn boarding-pass->seat-id
  [boarding-pass]
  (-> (->> (concat boarding-pass)
           (map ch->bit)
           (map str))
      string/join
      (Integer/parseInt 2)))



(defn available-seats
  [seat-ids]
  (let [available-seats (range (apply min seat-ids)
                               (apply max seat-ids))]
    (clojure.set/difference (set available-seats)
                            (set seat-ids))))



;; Journal
(comment

  (boarding-pass->seat-id "FBFBBFFRLR")
  #_=> 357


  ;; Part 1 answer
  (->> (util/file-as-seq "day5/input.txt")
       (map boarding-pass->seat-id)
       (apply max))
  #_=> 933

  ;; Part 2 answer
  (->> (util/file-as-seq "day5/input.txt")
       (map boarding-pass->seat-id)
       available-seats)
  #_=> #{711}

  )