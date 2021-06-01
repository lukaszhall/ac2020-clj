(ns ac2020-clj.day4-p2-spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::four-digit-str #(= 4 (count %)))

(defn in-year-range?
  [[range-start range-end] year-str]
  (let [year (or (try (Integer/parseInt year-str))
                 Integer/MIN_VALUE)]
    (<= range-start year range-end)))

(s/def ::byr (s/and ::four-digit-str
                    (partial in-year-range? [1920 2002])))


(s/def ::iyr (s/and ::four-digit-str
                    (partial in-year-range? [2010 2020])))

(s/def ::eyr (s/and ::four-digit-str
                    (partial in-year-range? [2020 2032])))

(defn valid-height-range?
  [height-str]
  (let [[_ v length-unit] (re-matches #"(\d+)(cm|in)" height-str)
        ;_ (println v)
        height (or (try (Integer/parseInt v))
                   Integer/MIN_VALUE)]
    (case length-unit
      "cm" (<= 150 height 193)
      "in" (<= 59 height 76))))

#_(s/def ::hgt valid-height-range?)
#_(Integer/parseInt nil)

(s/def ::valid-passport
  (s/keys :req-un [::byr
                   ::iyr
                   ::eyr
                   ::hgt
                   ::hcl
                   ::ecl
                   ::pid]
          :opt-un [::cid]))



(comment

  (map (partial s/valid? ::byr)
       ["1"
        "1234"
        "1941"
        "2010"
        "hi"])
  #_=> '(false false true false false)

  (map (partial s/valid? ::eyr)
       ["1"
        "1234"
        "1941"
        "2010"
        "hi"])

  (valid-height-range? "in1")

  (re-matches #"(\d+)(cm|in)" "60in")

  (map (partial s/valid? ::hgt)
       ["50in"
        "60in"
        "100in"
        "140cm"
        "160cm"
        "200cm"])
  #_=> '(false true false false true false)





  )