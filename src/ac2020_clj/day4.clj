(ns ac2020-clj.day4
  (:require [ac2020-clj.util :as util]
            [clojure.string :as string :refer [join]]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sg]
            [clojure.spec.test.alpha :as st]
            [clojure.test.check.generators :as test.gen]

            [ac2020-clj.day4-p1-spec :as p1-spec]
            [ac2020-clj.day4-p2-spec :as p2-spec]
            ))



(defn file-as-seq
  [filename]
  (-> (io/resource filename)
      slurp
      (string/split #"\n\n")))

(defn entry->passport
  "Convert a string rep to passport map"
  [entry]
  (->> (string/split entry #"\s")
       (map (fn [pair]
              (string/split pair #":")))
       (into {})
       (clojure.walk/keywordize-keys)))


(def valid-passport? s/valid?)

(defn passports-by-validity
  [validator-spec entry-s]
  (->> entry-s
       (map entry->passport)
       (group-by (partial valid-passport? validator-spec))))



(defn valid-passport-count
  [validator-spec entry-s]
  (-> (->> entry-s
           (passports-by-validity validator-spec))
      (get true)
      count))



(comment


  ;; Sample input seq
  (file-as-seq "day4/input_sample.txt")
  #_=> ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm"
        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929"
        "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm"
        "hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"]


  (entry->passport "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm")
  #_=> {:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"}

  (->> (file-as-seq "day4/input_sample.txt")
       (passports-by-validity ::p1-spec/valid-passport))
  #_=> {true  [{:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"}
               {:hcl "#ae17e1", :iyr "2013", :eyr "2024", :ecl "brn", :pid "760753108", :byr "1931", :hgt "179cm"}],
        false [{:iyr "2013", :ecl "amb", :cid "350", :eyr "2023", :pid "028048884", :hcl "#cfa07d", :byr "1929"}
               {:hcl "#cfa07d", :eyr "2025", :pid "166559648", :iyr "2011", :ecl "brn", :hgt "59in"}]}

  (->> (file-as-seq "day4/input_sample.txt")
       (valid-passport-count ::p1-spec/valid-passport))
  #_=> 2



  ;; Answer 1
  (->> (file-as-seq "day4/input.txt")
       (valid-passport-count ::p1-spec/valid-passport))
  #_=> 264

  ;; Answer 1
  (->> (file-as-seq "day4/input.txt")
       (valid-passport-count ::p2-spec/valid-passport))
  #_=> 111



  )
