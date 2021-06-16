(ns ac2020-clj.day6
  (:require [clojure.java.io :as io]
            [ac2020-clj.util :as util]
            [clojure.string :as string]))


(defn file-as-seq
  "Split sample file to seq of response seqs"
  [filename]
  (->> (-> (io/resource filename)
           slurp
           (string/split #"\n\n"))
       (map #(string/split % #"\n"))))

(def test-group-unique-yes
  (comp ))

(defn group-unique-yes
  "Count of unique questions answered yes within a group of answers"
  [group-answers]
  (->> group-answers
       (mapcat concat)
       distinct))

(defn group-intersected-yes
  "Count of questions answered yes for all answers"
  [group-answers]
  (->> group-answers
       (map set)
       (apply clojure.set/intersection)))


(comment
  ;; Sample input seq (sample run)
  (file-as-seq "day6/input_sample.txt")


  (group-unique-yes ["a" "abc" "da"])
  #_=> (\a \b \c \d)

  ;; sample
  (->> (file-as-seq "day6/input_sample.txt")
       (map group-unique-yes)
       (map count)
       (apply +))
  #_=> 11

  ;; part 1
  (->> (file-as-seq "day6/input.txt")
       (map group-unique-yes)
       (map count)
       (apply +))
  #_=> 6506


  (group-intersected-yes ["a" "abc" "da"])
  #_=> #{\a}

  ;; part 2
  (->> (file-as-seq "day6/input.txt")
       (map group-intersected-yes)
       (map count)
       (apply +))
  #_=> 3243

  )