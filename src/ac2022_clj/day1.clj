(ns ac2022-clj.day1
  (:require [ac2022-clj.util :as util]
            [medley.core :as m]
            [clojure.string :as string :refer [join]]))

;; Sample input seq (sample run)
(util/file-as-seq "ac2022/day1/input_sample.txt")
#_=> ["..##......."
      "#...#...#.."
      ".#....#..#."
      "..#.#...#.#"
      ".#...##..#."
      "..#.##....."
      ".#.#.#....#"
      ".#........#"
      "#.##...#..."
      "#...##....#"
      ".#..#...#.#"]


(defn colorie-strs->item-calories-by-elf-s
  "Convert "
  [calorie-strs-s]
  (->> calorie-strs-s
       (m/partition-before #{""})
       (mapv #(filterv (comp not #{""}) %))
       (mapv (fn [item-calorie-s]
               (mapv #(Integer/parseInt %) item-calorie-s)))))


(defn item-calories-by-elf-s->food-calories
  [item-calories-by-elf-s]
  (->> item-calories-by-elf-s
       (mapv #(reduce + 0 %))))



(comment


  (do (def input_sample
        (util/file-as-seq "ac2022/day1/input_sample.txt"))
      input_sample)
  #_=> ["1000" "2000" "3000" "" "4000" "" "5000" "6000" "" "7000" "8000" "9000" "" "10000"]


  (-> input_sample
      colorie-strs->item-calories-by-elf-s)
  #_=> [[1000 2000 3000] [4000] [5000 6000] [7000 8000 9000] [10000]]



  (-> input_sample
      colorie-strs->item-calories-by-elf-s
      item-calories-by-elf-s->food-calories)
  #_=> [6000 4000 11000 24000 10000]

  ;; Part 1 Answer
  (->> (util/file-as-seq "ac2022/day1/input.txt")
       colorie-strs->item-calories-by-elf-s
       item-calories-by-elf-s->food-calories
       (apply max))
  #_=> 75501

  ;; ------------

  (->> input_sample
       colorie-strs->item-calories-by-elf-s
       item-calories-by-elf-s->food-calories
       sort
       (take-last 3))
  #_=> '(10000 11000 24000)

  ;; Part 2 Answer
  (->> (util/file-as-seq "ac2022/day1/input.txt")
       colorie-strs->item-calories-by-elf-s
       item-calories-by-elf-s->food-calories
       sort
       (take-last 3)
       (reduce + 0))
  #_=> 215594

  )