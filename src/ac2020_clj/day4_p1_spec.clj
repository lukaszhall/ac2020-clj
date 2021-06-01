(ns ac2020-clj.day4-p1-spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::valid-passport
  (s/keys :req-un [::byr
                   ::iyr
                   ::eyr
                   ::hgt
                   ::hcl
                   ::ecl
                   ::pid]
          :opt-un [::cid]))