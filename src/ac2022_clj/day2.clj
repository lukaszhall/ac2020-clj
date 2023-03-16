(ns ac2022-clj.day2
  (:require [ac2022-clj.util :as util]
            [clojure.string :as string]))

;; Sample input seq (sample run)
(util/file-as-seq "ac2022/day2/input_sample.txt")
#_=> ["A Y" "B X" "C Z"]


;; Problem Domain

(def strat-decoder
  {"A" :rock
   "B" :paper
   "C" :scissors

   "X" :rock
   "Y" :paper
   "Z" :scissors})

(def shape-score
  {:rock     1
   :paper    2
   :scissors 3})

(def outcome-score
  {:win  6
   :draw 3
   :loss 0})

(def shape->losing-shapes
  {:paper    #{:rock}
   :scissors #{:paper}
   :rock     #{:scissors}})


;; Part 1 - Static Guide

(defn decode-strat-guide
  [decoder encodedstrat-s]
  (mapv (fn [encoded-strat]
          (->> (string/split encoded-strat #" ")
               (mapv decoder)))
        encodedstrat-s))


(defn round-result
  [winner-guide [opponent-shape your-shape :as _round]]
  (let [beats? (fn [p1-shape p2-shape] (-> (winner-guide p1-shape)
                                           (contains? p2-shape)))]
    (cond
      (beats? your-shape opponent-shape) :win
      (beats? opponent-shape your-shape) :loss
      :else :draw)))

(def basic-round-result (partial round-result shape->losing-shapes))


(defn shape-scores
  [round-s]
  (mapv (fn shape->score [[_opp-shape your-shape]]
          (shape-score your-shape))
        round-s))

(defn result-scores
  [round-s]
  (->> round-s
       (map basic-round-result)
       (mapv outcome-score)))

(defn add-scores
  [scoring-fns round-s]
  (->> scoring-fns
       (mapv (fn [scoring-fn] (scoring-fn round-s)))
       (apply mapv +)))


;; Part 2 - Responsive decoder Guide


(def result-decoder
  {"A" :rock
   "B" :paper
   "C" :scissors

   "X" :loss
   "Y" :draw
   "Z" :win})


(def shape->winning-shapes
  (->> shape->losing-shapes
       (mapv (fn [[accumulating-shape _]]
               [accumulating-shape (->> (filterv (fn [[_winner loser-shapes]]
                                                   (contains? loser-shapes accumulating-shape))
                                                 shape->losing-shapes)
                                        (mapv (fn [[winner _loser_shapes]] winner))
                                        (into #{}))]))
       (into {})))
#_=> {:paper #{:scissors}, :scissors #{:rock}, :rock #{:paper}}



(defn result-strat->shape-strat
  "Convert [opp-shape result] to [opp-shape your-shape]"
  [[opp-shape result]]
  (case result
    :loss
    [opp-shape (-> (shape->losing-shapes opp-shape) first)]

    :win
    [opp-shape (-> (shape->winning-shapes opp-shape) first)]

    :draw
    [opp-shape opp-shape]))

(defn result-guide->shape-guide
  [result-strat-s]
  (mapv result-strat->shape-strat result-strat-s))




(comment

  (do (def strat-guide-file (util/file-as-seq "ac2022/day2/input_sample.txt"))
      strat-guide-file)
  #_=> ["A Y" "B X" "C Z"]

  (do (def strat-guide (->> strat-guide-file
                            (decode-strat-guide strat-decoder)))
      strat-guide)
  #_=> [[:rock :paper] [:paper :rock] [:scissors :scissors]]

  (->> strat-guide
       (mapv basic-round-result))
  #_=> [:win :loss :draw]

  (->> strat-guide
       shape-scores)
  #_=> [2 1 3]

  (->> strat-guide
       result-scores)
  #_=> [6 0 3]

  (->> strat-guide
       (add-scores [shape-scores result-scores]))
  #_=> [8 1 6]

  (->> strat-guide
       (add-scores [shape-scores result-scores])
       (apply +))
  #_=> 15


  ;; Part 1 Answer
  (->> (util/file-as-seq "ac2022/day2/input.txt")
       (decode-strat-guide strat-decoder)
       (add-scores [shape-scores result-scores])
       (apply +))
  #_=> 9759


  ;; ------------------------------------------
  ;; Part 2

  (->> strat-guide-file
       (decode-strat-guide result-decoder))
  #_=> [[:rock :draw] [:paper :loss] [:scissors :win]]


  (->> strat-guide-file
       (decode-strat-guide result-decoder)
       result-guide->shape-guide)
  #_=> [[:rock :rock] [:paper :rock] [:scissors :rock]]

  (->> strat-guide-file
       (decode-strat-guide result-decoder)
       result-guide->shape-guide
       (add-scores [shape-scores result-scores])
       (apply +))
  #_=> 12


  ;; Part 2 Answer
  (->> (util/file-as-seq "ac2022/day2/input.txt")
       (decode-strat-guide result-decoder)
       result-guide->shape-guide
       (add-scores [shape-scores result-scores])
       (apply +))
  #_=> 12429

  )