(ns ac2022-clj.day3
  (:require [ac2022-clj.util :as util]
            [clojure.string :as string]))


;; Sample input seq (sample run)
(util/file-as-seq "ac2022/day3/input_sample.txt")
#_=> ["vJrwpWtwJgWrhcsFMMfFFhFp"
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
      "PmmdzqPrVvPwwTWBwg"
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
      "ttgJtRGJQctTZtZT"
      "CrZsJsPPZsGzwwsLwLmpwMDw"]



;; Problem domain

(defn str->rucksacks
  "Convert input file format into a rucksack"
  [rucksack-str-s]
  (->> rucksack-str-s
       (mapv (fn rucksack-str->rucksack [r-str]
               (let [compartment-size (/ (count r-str)
                                         2)
                     compartmentalize (fn [low high]
                                        (-> (subs r-str low high)
                                            vec))]
                 [(compartmentalize 0 compartment-size)
                  (compartmentalize compartment-size (count r-str))])))))

(defn common-compartment-items
  "Convert a rucksack-s into a seq of vectors containing shared items in a given
  rucksack's compartments"
  [rucksack-s]
  (->> rucksack-s
       (mapv (fn common-items [compartments]
               (->> compartments
                    (map set)
                    (apply clojure.set/intersection))))))

(def item->priority
  (let [a-zA-Z (map char (concat (range (int \a)
                                        (inc (int \z)))
                                 (range (int \A)
                                        (inc (int \Z)))))]
    (zipmap a-zA-Z
            (range 1 (inc (count a-zA-Z))))))
#_=> {\A 27,
      \a 1,
      \B 28,
      \b 2
      #_.....
      \Y 51,
      \y 25,
      \Z 52,
      \z 26}


(defn shareditem-priority
  [common-compartment-items]
  (->> common-compartment-items
       (mapv #(mapv item->priority %))))



(defn str->groupsacks
  "Convert input file format into a group rucksacks"
  [rucksack-str-s]
  (->> rucksack-str-s
       (mapv vec)
       (partition 3)

       ;; convert '(()) to [[]]
       (mapv vec)))



(comment
  (do (def rucksack-file-sample (util/file-as-seq "ac2022/day3/input_sample.txt"))
      rucksack-file-sample)
  #_=> ["vJrwpWtwJgWrhcsFMMfFFhFp"
        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
        "PmmdzqPrVvPwwTWBwg"
        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
        "ttgJtRGJQctTZtZT"
        "CrZsJsPPZsGzwwsLwLmpwMDw"]

  (do (def rucksack-sample (str->rucksacks rucksack-file-sample))
      rucksack-sample)
  #_=> [[[\v \J \r \w \p \W \t \w \J \g \W \r] [\h \c \s \F \M \M \f \F \F \h \F \p]]
        [[\j \q \H \R \N \q \R \j \q \z \j \G \D \L \G \L] [\r \s \F \M \f \F \Z \S \r \L \r \F \Z \s \S \L]]
        [[\P \m \m \d \z \q \P \r \V] [\v \P \w \w \T \W \B \w \g]]
        [[\w \M \q \v \L \M \Z \H \h \H \M \v \w \L \H] [\j \b \v \c \j \n \n \S \B \n \v \T \Q \F \n]]
        [[\t \t \g \J \t \R \G \J] [\Q \c \t \T \Z \t \Z \T]]
        [[\C \r \Z \s \J \s \P \P \Z \s \G \z] [\w \w \s \L \w \L \m \p \w \M \D \w]]]

  (common-compartment-items [[[:a :b :c] [:b :c] [:c :d]]
                             [[:a :b :c] [:b]]])
  #_=>  [#{:c} #{:b}]

  (common-compartment-items rucksack-sample)
  #_=> [#{\p} #{\L} #{\P} #{\v} #{\t} #{\s}]

  (->> [#{\p \z} #{\a}]
       (mapv #(mapv item->priority %)))
  #_=> [[16 26] [1]]
  (apply + (flatten [[16 26] [1]]))
  #_ => 43


  ;; Problem 1 Answer
  (->> (util/file-as-seq "ac2022/day3/input.txt")
       str->rucksacks
       common-compartment-items
       shareditem-priority
       flatten
       (apply +))
  #_=> 7863

  ;; Problem 2
  (do (def groupsack-sample (str->groupsacks rucksack-file-sample))
      groupsack-sample)
  #_=> [[[\v \J \r \w \p \W \t \w \J \g \W \r \h \c \s \F \M \M \f \F \F \h \F \p]
         [\j \q \H \R \N \q \R \j \q \z \j \G \D \L \G \L \r \s \F \M \f \F \Z \S \r \L \r \F \Z \s \S \L]
         [\P \m \m \d \z \q \P \r \V \v \P \w \w \T \W \B \w \g]]
        [[\w \M \q \v \L \M \Z \H \h \H \M \v \w \L \H \j \b \v \c \j \n \n \S \B \n \v \T \Q \F \n]
         [\t \t \g \J \t \R \G \J \Q \c \t \T \Z \t \Z \T]
         [\C \r \Z \s \J \s \P \P \Z \s \G \z \w \w \s \L \w \L \m \p \w \M \D \w]]]

  (common-compartment-items groupsack-sample)
  #_=> [#{\r} #{\Z}]

  ;; Part 2 Answer
  (->> (util/file-as-seq "ac2022/day3/input.txt")
       str->groupsacks
       common-compartment-items
       shareditem-priority
       flatten
       (apply +))
  #_=> 2488

  )