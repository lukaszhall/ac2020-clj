(ns ac2022-clj.day6
  (:require [ac2022-clj.util :as util]
            [clojure.string :as string]))


(defn uniq?
  "true iff chars in the substring are all unique"
  [signal-substr]
  (when (-> signal-substr
            (-> set count)
            (= (count signal-substr)))
    signal-substr))



(defn first-marker
  "Return the marker as identified by pred found in signal of marker-size"
  [marker-size pred signal]
  (->> (partition marker-size 1 signal)
       (map string/join)
       (some pred)))


(def first-uniq4-marker (partial first-marker 4 uniq?))

(defn marker-location
  "Marker location is the first character index AFTER the marker in the signal"
  [signal marker]
  (+ (count marker)
     (string/index-of signal marker)))


;; Part 2

(def first-uniq14-marker (partial first-marker 14 uniq?))


















;; Rich Comment Block
(comment

  (do (def sample-inputs (util/file-as-seq "ac2022/day6/input_sample.txt"))
      sample-inputs)
  #_=> ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"
        "bvwbjplbgvbhsrlpgdmjqwftvncz"
        "nppdvjthqldpwncqszvftbrmjlhg"
        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]

  (mapv uniq? ["abcd"
               "abca"
               ""])
  #_=> ["abcd" nil ""]



  (some uniq? ["aaa" "bbbb" "aba" "cd" "eee" "ab"])
  #_=> "cd"

  (->> (first sample-inputs)
       (first-marker 4 uniq?))
  #_=> "jpqm"


  (let [sample (first sample-inputs)]
    (->> sample
         (first-uniq4-marker)
         (marker-location sample)))
  #_=> 7


  (mapv (juxt identity #(->> %
                             (first-uniq4-marker)
                             (marker-location %)))
        sample-inputs)
  #_=> [["mjqjpqmgbljsphdztnvjfqwrcgsmlb" 7]
        ["bvwbjplbgvbhsrlpgdmjqwftvncz" 5]
        ["nppdvjthqldpwncqszvftbrmjlhg" 6]
        ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10]
        ["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 11]]


  (partition 4 1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")


  ;; Part 1 answer

  (let [input-signal (-> (util/file-as-seq "ac2022/day6/input.txt")
                         first)]
    (->> input-signal
         (first-uniq4-marker)
         (marker-location input-signal)))
  #_=> 1538



  ;; Part 2 answer

  (let [input-signal (-> (util/file-as-seq "ac2022/day6/input.txt")
                         first)]
    (->> input-signal
         (first-marker 14 uniq?)
         (marker-location input-signal)))
  #_=> 2315

  )










