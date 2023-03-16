(ns ac2022-clj.day4
  (:require [ac2022-clj.util :as util]
            [clojure.string :as str]))



;; Sample input seq (sample run)
(util/file-as-seq "ac2022/day4/input_sample.txt")
#_=> ["2-4,6-8" "2-3,4-5" "5-7,7-9" "2-8,3-7" "6-6,4-6" "2-6,4-8"]


(defn assignment-str->assignment-pair
  "xform section strings to pairs of ranges, e.g., [[3 4] [6 8]]"
  [section-str]
  (let [range-strs                   (str/split section-str #",")
        section-str->range-vector-fn (fn [range-str]
                                       (->> (str/split range-str #"-")
                                            (mapv #(Integer/parseInt %))))]
    (mapv section-str->range-vector-fn range-strs)))


(defn assignment-strs->assignment-pairs
  [section-strs]
  (mapv assignment-str->assignment-pair section-strs))


(defn range-subset?
  "true iff first range is inclusively included in second range"
  [[range-lower range-upper :as _range]
   [superset-lower superset-upper :as _superset-range]]
  (and (<= superset-lower range-lower)
       (>= superset-upper range-upper)))


(defn some-pred-pair
  "true iff application of the 2-arg pred to [range1 range2] or [range2 range1] true"
  [pred [range1 range2 :as _assignment-pair]]
  (some (fn [[r1 r2]]
          (pred r1 r2))
        [[range1 range2]
         [range2 range1]]))



(defn range-overlap?
  "true iff first range overlaps second range"
  [[r1-lower r1-upper :as _range1]
   [r2-lower r2-upper :as _range2]]
  (or
    ;;  r2  ...... [---]....
    ;;  r1  ....[---].......
    (and (<= r2-lower r1-upper)
         (>= r2-upper r1-lower))
    ;;  r2  ... [---]....
    ;;  r1  .......[---].......
    (and (<= r1-lower r2-upper)
         (>= r1-upper r2-lower))))



(comment

  (do (def sample-input (util/file-as-seq "ac2022/day4/input_sample.txt"))
      sample-input)
  #_=> ["2-4,6-8" "2-3,4-5" "5-7,7-9" "2-8,3-7" "6-6,4-6" "2-6,4-8"]


  (assignment-str->assignment-pair "2-4,6-8")
  #_=> [[2 4] [6 8]]

  (assignment-strs->assignment-pairs sample-input)
  #_=> [[[2 4] [6 8]] [[2 3] [4 5]] [[5 7] [7 9]] [[2 8] [3 7]] [[6 6] [4 6]] [[2 6] [4 8]]]

  ;; Verify range logic
  (let [ident->range-subset? (fn [[r1 r2]] [[r1 r2] "=>" (range-subset? r1 r2)])
        base-lower           2
        base-upper           4
        base-range           [base-lower base-upper]]
    (mapv ident->range-subset? [[base-range base-range]
                                [base-range [(dec base-lower) base-upper]]
                                [base-range [base-lower (inc base-upper)]]
                                [[(dec base-lower) base-upper] base-range]
                                [[base-lower (inc base-upper)] base-range]]))
  #_=> [[[[2 4] [2 4]] "=>" true]
        [[[2 4] [1 4]] "=>" true]
        [[[2 4] [2 5]] "=>" true]
        [[[1 3] [2 4]] "=>" false]
        [[[2 5] [2 4]] "=>" false]]

  (mapv (partial some-pred-pair =) [[2 3]
                                    [1 1]
                                    [4 1]])
  #_=> [nil true nil]

  ;; Part 1 sample
  (->> sample-input
       assignment-strs->assignment-pairs
       (mapv (partial some-pred-pair range-subset?))
       (filter (comp not nil?))
       count)
  #_=> 2

  ;; Part 1 answer
  (->> (util/file-as-seq "ac2022/day4/input.txt")
       assignment-strs->assignment-pairs
       (mapv (partial some-pred-pair range-subset?))
       (filter (comp not nil?))
       count)
  #_=> 500


  ;; Verify overlap logic
  (let [ident->range-overlap? (fn [[r1 r2]] [[r1 r2] "=>" (range-overlap? r1 r2)])
        base-lower            5
        base-upper            8
        base-range            [base-lower base-upper]]
    (mapv ident->range-overlap? [[base-range base-range]
                                 [base-range [(dec base-lower) base-upper]]
                                 [base-range [base-lower (inc base-upper)]]
                                 [base-range [base-upper base-upper]]
                                 [base-range [base-upper (inc base-upper)]]
                                 [base-range [(inc base-upper) (inc (inc base-upper))]]
                                 [base-range [base-lower base-lower]]
                                 [base-range [(dec base-lower) base-lower]]
                                 [base-range [(dec (dec base-lower)) (dec base-lower)]]]))
  #_=> [[[[5 8] [5 8]] "=>" true]
        [[[5 8] [4 8]] "=>" true]
        [[[5 8] [5 9]] "=>" true]
        [[[5 8] [8 8]] "=>" true]
        [[[5 8] [8 9]] "=>" true]
        [[[5 8] [9 10]] "=>" false]
        [[[5 8] [5 5]] "=>" true]
        [[[5 8] [4 5]] "=>" true]
        [[[5 8] [3 4]] "=>" false]]


  ;; Part 2 answer
  (->> (util/file-as-seq "ac2022/day4/input.txt")
       assignment-strs->assignment-pairs
       (mapv (partial some-pred-pair range-overlap?))
       (filter (comp not nil?))
       count)
  #_=> 815

  )






(comment



  (def strs ["flower", "flow", "flight"])
  (let [base-word  (get strs 0)
        word-chars (->> strs
                        (mapv vec))
        end-idx    (loop [i 0]
                     (let [i-chars (mapv #(get % i) word-chars)]
                       (if-not (apply = i-chars)
                         i
                         (recur (inc i)))))]
    (->> (subvec (vec base-word)
                 0
                 end-idx)
         (clojure.string/join))))




