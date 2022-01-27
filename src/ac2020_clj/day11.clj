(ns ac2020-clj.day11
  (:require [ac2020-clj.util :as util]
            [clojure.string :as str]))

;;sample input_sample
(util/file-as-seq "day11/input_sample.txt")
#_=> ["L.LL.LL.LL"
      "LLLLLLL.LL"
      "L.L.L..L.."
      "LLLL.LL.LL"
      "L.LL.LL.LL"
      "L.LLLLL.LL"
      "..L.L....."
      "LLLLLLLLLL"
      "L.LLLLLL.L"
      "L.LLLLL.LL"]

(defn str-grid->sym-grid
  "Convert grid with string rows to 2d symb grid"
  [str-grid]
  (mapv (fn [grid-row]
          (->> (mapv {\L 'L
                      \. '.
                      \# 'O}
                     grid-row)))
        str-grid))

(defn sym-grid->str-grid
  "For display. Convert Sym grid to string grid. Inversion of str-grid->sym-grid"
  [sym-grid]
  (mapv (fn [grid-row] (->> (map {'L \L
                                  '. \.
                                  'O \#}
                                 grid-row)
                            str/join))
        sym-grid))

(defn grid-rows [sym-grid] (count sym-grid))
(defn grid-cols [sym-grid] (count (first sym-grid)))


(defn in-grid?
  "true iff location is a valid point in the grid, where location is [row col]. Assumes all rows are equal length"
  [sym-grid location]
  (let [[row col] location]
    (and (< -1 row (grid-rows sym-grid))
         (< -1 col (grid-cols sym-grid)))))

(defn grid-val
  "Value at specified sym-grid location"
  [sym-grid location]
  (let [[row col] location]
    (-> sym-grid
        (get row)
        (get col))))


(defn nearest-neighbor-fn
  "Return neighbor function for nearest neighbor in direction, where direction is [row col], else nil"
  [direction]
  (let [[dir-row dir-col] direction]
    (fn [sym-grid loc]
      (let [[row col] loc
            neighbor-loc [(+ row dir-row) (+ col dir-col)]]
        (if-not (in-grid? sym-grid neighbor-loc)
          nil
          (grid-val sym-grid neighbor-loc)))))
  )

(def nearest-neighbors-fns (map nearest-neighbor-fn [[-1 -1]
                                                     [-1 0]
                                                     [-1 1]
                                                     [0 1]
                                                     [1 1]
                                                     [1 0]
                                                     [1 -1]
                                                     [0 -1]]))

(defn neighbors
  "Find seq of all neighbors using the neighbor searching functions"
  [sym-grid location neighbor-fn-s]
  (let [found-neighbors (->> (mapv (fn [f] (f sym-grid location))
                                   neighbor-fn-s)
                             (filterv some?))]
    found-neighbors))


(defn iterate-seat-p1
  "Get value of seat in it's next version"
  [sym-grid loc]
  (let [seat-val                (grid-val sym-grid loc)
        ;_ (println seat-val)
        neighbors               (neighbors sym-grid loc nearest-neighbors-fns)
        ;_                       (println neighbors)
        occupied-neighbor-count (->> neighbors
                                     (filter #(= % 'O))
                                     count)
        ;_ (println occupied-neighbor-count)
        ;_ (println (str (>= occupied-neighbor-count 4)))
        ]
    (case seat-val
      . '.
      L (if (zero? occupied-neighbor-count)
          'O
          'L)
      O (if (>= occupied-neighbor-count 4)
          'L
          'O))))

(defn iterated-grid
  "Single iteration of a grid using supplied seat-iterator-fn"
  [seat-iterator-fn sym-grid]
  (mapv (fn [row]
          (mapv (fn [col] (seat-iterator-fn sym-grid [row col]))
                (range 0 (grid-cols sym-grid))))
        (range 0 (grid-rows sym-grid))))

(defn stable-grid
  "Iterate grid until non-changing"
  [sym-grid seat-iterator-fn]
  (-> (->> sym-grid
           (iterate (partial iterated-grid seat-iterator-fn))
           (partition 2 1)
           (take-while (fn [[prev-iter next-iter]]
                         (not= prev-iter next-iter)))
           last)
      first))

(defn occupied-count
  [sym-grid]
  (reduce + 0
          (map (fn [grid-row]
                 (->> grid-row
                      (filter #(= % 'O))
                      count))
               sym-grid))
  )

(comment

  (do (def input-sample (->> (util/file-as-seq "day11/input_sample.txt")
                             str-grid->sym-grid))
      (def input (->> (util/file-as-seq "day11/input.txt")
                      str-grid->sym-grid))
      input-sample
      )
  #_=> [[L . L L . L L . L L]
        [L L L L L L L . L L]
        [L . L . L . . L . .]
        [L L L L . L L . L L]
        [L . L L . L L . L L]
        [L . L L L L L . L L]
        [. . L . L . . . . .]
        [L L L L L L L L L L]
        [L . L L L L L L . L]
        [L . L L L L L . L L]]


  (sym-grid->str-grid input-sample)
  #_=> ["L.LL.LL.LL"
        "LLLLLLL.LL"
        "L.L.L..L.."
        "LLLL.LL.LL"
        "L.LL.LL.LL"
        "L.LLLLL.LL"
        "..L.L....."
        "LLLLLLLLLL"
        "L.LLLLLL.L"
        "L.LLLLL.LL"]

  ;; in-grid? test
  (let [grid-test input-sample]
    (into [] (for [row (range -1 (inc (count grid-test)))]
               (into [] (for [col (range -1 (inc (count (get grid-test 0))))]
                          (-> (in-grid? grid-test [row col])
                              {true 't false 'f}))))))
  #_=> [[f f f f f f f f f f f f]
        [f t t t t t t t t t t f]
        [f t t t t t t t t t t f]
        [f t t t t t t t t t t f]
        [f t t t t t t t t t t f]
        [f t t t t t t t t t t f]
        [f t t t t t t t t t t f]
        [f t t t t t t t t t t f]
        [f t t t t t t t t t t f]
        [f t t t t t t t t t t f]
        [f t t t t t t t t t t f]
        [f f f f f f f f f f f f]]

  ;; grid-val test
  (let [grid-test input-sample]
    (into [] (for [row (range 0 (count grid-test))]
               (into [] (for [col (range 0 (count (get grid-test 0)))]
                          (grid-val grid-test [row col]))))))
  #_=> [[L . L L . L L . L L]
        [L L L L L L L . L L]
        [L . L . L . . L . .]
        [L L L L . L L . L L]
        [L . L L . L L . L L]
        [L . L L L L L . L L]
        [. . L . L . . . . .]
        [L L L L L L L L L L]
        [L . L L L L L L . L]
        [L . L L L L L . L L]]

  ;; test neighbors, nearest neighbors
  [(neighbors input-sample [1 1] nearest-neighbors-fns)
   (neighbors input-sample [0 0] nearest-neighbors-fns)
   (neighbors input-sample [0 1] nearest-neighbors-fns)
   (neighbors input-sample [9 1] nearest-neighbors-fns)]
  #_=> [[L . L L L . L L] [. L L] [L L L L L] [L . L L L]]

  (iterate-seat-p1 input-sample [1 1])
  #_=> O

  (iterate-grid input-sample iterate-seat-p1)
  #_=> [[O . O O . O O . O O]
        [O O O O O O O . O O]
        [O . O . O . . O . .]
        [O O O O . O O . O O]
        [O . O O . O O . O O]
        [O . O O O O O . O O]
        [. . O . O . . . . .]
        [O O O O O O O O O O]
        [O . O O O O O O . O]
        [O . O O O O O . O O]]

  (->> (stable-grid input-sample iterate-seat-p1)
       occupied-count)


  (def iter2 (iterated-grid iterate-seat-p1 input-sample))

  (iterate-seat-p1 iter2 [0 9])
  )

