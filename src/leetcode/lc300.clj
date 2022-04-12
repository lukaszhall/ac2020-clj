(ns leetcode.lc300
  (:import (java.util Comparator)))

(comment
  " 300. Longest Increasing Subsequence
    Medium

    Given an integer array nums, return the length of the longest strictly increasing subsequence.
    A subsequence is a sequence that can be derived from an array by deleting some or no elements without changing the order of the remaining elements. For example, [3,6,2,7] is a subsequence of the array [0,3,1,6,2,2,7].

    Example 1:
      Input: nums = [10,9,2,5,3,7,101,18]
      Output: 4
      Explanation: The longest increasing subsequence is [2,3,7,101], therefore the length is 4.

    Example 2:
      Input: nums = [0,1,0,3,2,3]
      Output: 4

    Example 3:
      Input: nums = [7,7,7,7,7,7,7]
      Output: 1

    Constraints:         1 <= nums.length <= 2500   |   -104 <= nums[i] <= 104

    Follow up: Can you come up with an algorithm that runs in O(n log(n)) time complexity?")




(defn- binary-search
  "binary search in vector for index with an idx in less than the target. Optionally supplied value-xform-fn to extract
  value from vals if it must be transformed"
  ([vals target]
   (binary-search vals target identity))
  ([vals target value-xform-fn]
   (loop [low  0
          high (dec (count vals))]
     (if (not (<= low high))
       low
       (let [idx (int (Math/floor (/ (+ low high) 2)))
             v   (value-xform-fn (nth vals idx))]
         (cond
           (= v target) idx
           (< v target) (recur (inc idx) high)
           (> v target) (recur low (dec idx))))))))



(defn longest-increasing-subseq
  "Returns the longest increasing subseq in O(n lg n) time and O(n) space"
  [nums]
  (loop [;; vec of {:nums-val, :nums-idx}. vec[j] stores the nums idx and val for the longest subsequence of length j.
         ;;
         ;; A vec[3] of {:val 4 :idx 5} indicates that the smallest value for a subsequence of length 3 can be found
         ;; terminating at num[5] with a value of 4
         lis-answers [{:num-idx 0 :num-val 0}]
         idx         0]

    (if (>= idx (count nums))
      lis-answers
      (let [next-val                    (nth nums idx)
            lis-answers-idx-of-next-val (-> (binary-search lis-answers
                                                           next-val
                                                           :num-val)
                                            (Math/max 0))]
        (cond
          (== lis-answers-idx-of-next-val (count lis-answers))
          (recur (conj lis-answers {:num-idx idx :num-val next-val})
                 (inc idx))

          ;; when it's lower than the nth value, replace with this lower (better) value
          (< next-val (:num-val (nth lis-answers lis-answers-idx-of-next-val)))
          (recur (assoc lis-answers lis-answers-idx-of-next-val {:num-idx idx :num-val next-val})
                 (inc idx))

          ;; else, answer isn't any better, move on
          :else
          (recur lis-answers
                 (inc idx)))))))




(comment

  ;; Verify binary-search
  (let [targets [41 51 7 8 9]]
    (->> (map (partial binary-search [0 2 4 7 9 11 13 15 17 27 40 50])
              targets)
         (zipmap targets)))
  #_=> {41 11, 51 12, 7 3, 8 4, 9 4}


  (binary-search [] 1)
  #_=> -1

  (binary-search [0 5 10 15 20 25 30 35 40] 41)



  ;; Verify Binary search for wrapped data
  (let [targets [7 19 3 27]]
    (->> (map (fn [target] (binary-search [{:num-idx 4 :num-val 4}
                                           {:num-idx 3 :num-val 7}
                                           {:num-idx 5 :num-val 10}
                                           {:num-idx 7 :num-val 19}
                                           {:num-idx 9 :num-val 25}]
                                          target
                                          :num-val))
              targets)
         (zipmap targets)))
  #_=> {7 1, 19 3, 3 -1, 27 4}


  (binary-search [{:num-val 0, :num-idx 0} {:num-idx 0, :num-val 3} {:num-idx 1, :num-val 5}]
                 4
                 :num-val)

  (->>
    (longest-increasing-subseq (->> [3 7 9 4 21 22 23 2 10 11 12 13]
                                       (take 6)
                                       (into [])))
       rest
       (map :num-val)
       (into []))


  (java.util.Collections/binarySearch [1 2 5 7 9 ] 0 )

  )

