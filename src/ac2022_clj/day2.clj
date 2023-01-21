(ns ac2022-clj.day2
  (:require [ac2022-clj.util :as util]))


(def char->shape
  {"A" :rock
   "B" :paper
   "C" :scissors

   "X" :rock
   "Y" :paper
   "Z" :scissors})


(defn file->strat-m
  [file-s]

  )






(comment

  (do (def strat-guide-file (util/file-as-seq "ac2022/day2/input_sample.txt"))
      strat-guide-file)
  #_=> ["A Y" "B X" "C Z"]

  )