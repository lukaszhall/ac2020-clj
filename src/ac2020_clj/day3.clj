(ns ac2020-clj.day3
  (:require [ac2020-clj.util :as util]
            [clojure.string :as string :refer [join]]))


;; Sample input seq (sample run)
(util/file-as-seq "day3/input_sample.txt")
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



(defn tree? [ch] (= ch \#))
(def clear? (complement tree?))



(defn terrain-pattern->terrain
  "Lazy infinite cycle of terrain pattern as chars"
  [terrain-lines]
  (->> terrain-lines
       (map concat)
       (map cycle)))



(defn slope-on-terrain->path
  "Given a 'slope' (steps 'right' per descent), give a char path to the bottom of the mountain"
  [[steps-over-per-descent descent] terrain]
  (->> terrain
       (take-nth descent)
       (map (fn [row terrain-line]
              (nth terrain-line
                   (* row
                      steps-over-per-descent)))
            (range))))


(defn tree-count
  "For a given path, return tree count"
  [path]
  (->> path
       (map tree?)
       (filter true?)
       count))



(defn tree-count-for-terrain-with-slope
  "Count of trees encountered for a terrain-pattern and slope"
  [terrain-pattern [steps-over-per-descent descent]]
  (->> terrain-pattern
       terrain-pattern->terrain
       (slope-on-terrain->path [steps-over-per-descent descent])
       tree-count))


(defn tree-product-for-terrain-with-slopes
  "Product of trees encountered for a single terrain, given different slopes"
  [terrain-pattern slopes]
  (let [tree-count-with-slope-fn (partial tree-count-for-terrain-with-slope
                                          terrain-pattern)]
    (->> (map tree-count-with-slope-fn
              slopes)
         (apply *))))


;; Journal
(comment

  (do (def sample-terrain (util/file-as-seq "day3/input_sample.txt"))
      sample-terrain)
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


  [(map tree? [\# \? \.])
   (map clear? [\# \? \.])]
  #_=> ['(true false false)
        '(false true true)]


  ;; terrain-patter->terrain
  (->> (util/file-as-seq "day3/input_sample.txt")
       terrain-pattern->terrain
       (map #(take 102 %))
       (map string/join))
  #_=> '("..##.........##.........##.........##.........##.........##.........##.........##.........##........"
          "#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#"
          ".#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.."
          "..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#."
          ".#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.."
          "..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.......#.##.......#.##.......#.##......"
          ".#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#."
          ".#........#.#........#.#........#.#........#.#........#.#........#.#........#.#........#.#........#."
          "#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#"
          "#...##....##...##....##...##....##...##....##...##....##...##....##...##....##...##....##...##....##"
          ".#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.")

  ;; slope-on-terrain->path
  (->> (util/file-as-seq "day3/input_sample.txt")
       terrain-pattern->terrain
       (slope-on-terrain->path [3 1]))
  #_=> '(\. \. \# \. \# \# \. \# \# \# \#)

  (->> (util/file-as-seq "day3/input_sample.txt")
       terrain-pattern->terrain
       (slope-on-terrain->path [1 1]))
  #_=> '(\. \. \. \. \. \# \. \. \. \. \#)

  (->> (util/file-as-seq "day3/input_sample.txt")
       terrain-pattern->terrain
       (slope-on-terrain->path [1 2]))
  #_=> '(\. \# \# \. \# \#)


  ;; tree-count
  (->> (util/file-as-seq "day3/input_sample.txt")
       terrain-pattern->terrain
       (slope-on-terrain->path [3 1])
       tree-count)
  #_=> 7

  ;; trees-for-slope
  (tree-count-for-terrain-with-slope (util/file-as-seq "day3/input_sample.txt")
                                     [3 1])
  #_=> 7


  ;; Problem 1 solution
  (tree-count-for-terrain-with-slope (util/file-as-seq "day3/input.txt")
                                     [3 1])
  #_=> 159





  ;; tree-product-for-terrain-with-slopes
  (tree-product-for-terrain-with-slopes (util/file-as-seq "day3/input_sample.txt")
                                        [[1 1]
                                         [3 1]
                                         [5 1]
                                         [7 1]
                                         [1 2]])
  #_=> 336

  ;; Problem 2 solution
  (tree-product-for-terrain-with-slopes (util/file-as-seq "day3/input.txt")
                                        [[1 1]
                                         [3 1]
                                         [5 1]
                                         [7 1]
                                         [1 2]])
  #_=> 6419669520
  )