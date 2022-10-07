(ns ac2020-clj.day2
  (:require [ac2020-clj.util :as util]
            [clojure.string :as str]))


;; Sample input seq (db entry strings)
(util/file-as-seq "day2/input_sample.txt")
#_=> ["1-3 a: abcde"
      "1-3 b: cdefg"
      "2-9 c: ccccccccc"]

(defn str->db-entry
  [db-entry-str]
  (let [[_ lmin lmax [letter] password] (re-matches #"(\d+)-(\d+) ([a-z]): ([a-z]*)" db-entry-str)]
    {:letter-min (Integer/parseInt lmin)
     :letter-max (Integer/parseInt lmax)
     :letter     letter
     :password   password}))

(defn valid-password?
  [{:as _db-entry
    :keys [letter-min letter-max letter password]}]
  (let [letter-count (-> (frequencies password)
                         (get letter)
                         (or 0))]
    (<= letter-min
        letter-count
        letter-max)))

(defn valid-password-count
  [db-entry-file]
  (->> (util/file-as-seq db-entry-file)
       (map str->db-entry)
       (filter valid-password?)
       count))

;; Part 2

(defn db-entry->db-positional-entry
  "Remap to naming scheme for part 2"
  [{:as _db-entry :keys [letter-min letter-max letter password]}]
  {:first-idx  (dec letter-min)
   :second-idx (dec letter-max)
   :letter     letter
   :password   password})


(defn valid-positional-password?
  [{:as _db-positional-entry :keys [first-idx second-idx letter password]}]
  (let [either-or            not=
        char-at-idx-matches? (fn [idx] (= (nth password idx)
                                          letter))]
    (either-or (char-at-idx-matches? first-idx)
               (char-at-idx-matches? second-idx))))


(defn valid-positional-password-count
  [db-entry-file]
  (->> (util/file-as-seq db-entry-file)
       (map str->db-entry)
       (map db-entry->db-positional-entry)
       (filter valid-positional-password?)
       count))




(comment
  (map str->db-entry (util/file-as-seq "day2/input_sample.txt"))
  #_=> ({:letter-min 1, :letter-max 3, :letter \a, :password "abcde"}
        {:letter-min 1, :letter-max 3, :letter \b, :password "cdefg"}
        {:letter-min 2, :letter-max 9, :letter \c, :password "ccccccccc"})

  (valid-password? {:letter-min 2, :letter-max 9, :letter \c, :password "ccccccccc"})
  #_=> false

  (->> (util/file-as-seq "day2/input_sample.txt")
       (map str->db-entry)
       (map valid-password?))
  #_=> '(true false true)

  ;; Part 1 Answer
  (valid-password-count "day2/input.txt")
  #_=> 456

  ;; Part 2
  (->> (util/file-as-seq "day2/input_sample.txt")
       (map str->db-entry)
       (map db-entry->db-positional-entry))
  #_=> ({:first-idx 0, :second-idx 2, :letter \a, :password "abcde"}
        {:first-idx 0, :second-idx 2, :letter \b, :password "cdefg"}
        {:first-idx 1, :second-idx 8, :letter \c, :password "ccccccccc"})

  (->> (util/file-as-seq "day2/input_sample.txt")
       (map str->db-entry)
       (map db-entry->db-positional-entry)
       (map valid-positional-password?))
  #_=> '(true false false)

  ;; Part 2 Answer
  (valid-positional-password-count "day2/input.txt")
  #_=> 308

  )