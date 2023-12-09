(ns day.7
  (:require [clojure.string :as str]))

(def values {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \Q 12 \K 13 \A 14
             \J 1 }) ;part 2

(defn to-count [acc num]
  (if (contains? acc num) (update acc num inc)
      (assoc acc num 1)))

(defn count-comparison [[f1 s1] [f2 s2]]
  (let [comp (compare s2 s1)]
    (if-not (zero? comp) comp
            (compare f2 f1))))

(defn treat-p2 [values]
  (let [joker-index (.indexOf (mapv first values) 1)]
    (if (neg? joker-index) values
        (let [joker-filtered (vec (concat (subvec values 0 joker-index) (subvec values (inc joker-index) (count values))))]
          (if-not (first joker-filtered) values 
                  (update-in joker-filtered [0 1] + (second (values joker-index))))))))

(defn treat-hand [h]
  (->> (mapv values h)
       (reduce to-count {})
       (sort count-comparison)
       (vec)
       (treat-p2)))

(defn parse [s]
  (let [tokens (str/split s #" ")
        hand-token (first tokens)
        score-token (read-string (last tokens))]
    {:hand (treat-hand hand-token) :original hand-token :score score-token  }))

(defn hand-level [h]
  (case (mapv second h)
    [5] 7
    [4 1] 6
    [3 2] 5
    [3 1 1] 4
    [2 2 1] 3
    [2 1 1 1] 2
    [1 1 1 1 1] 1))

(defn better-hand [c1 c2]
  (let [comp (compare (hand-level (:hand c1)) (hand-level (:hand c2)))]
    (if-not (zero? comp) comp
            (or (->> (map #(compare (values %1) (values %2)) (:original c1) (:original c2))
                     (filterv #(not (zero? %)))
                     (first))
                0))))

(defn solve [s]
  (let [tokens (str/split-lines (slurp s))]
    (->> (mapv parse tokens)
         (sort better-hand)
         (mapv :score)
         (map #(vector %1 %2) (range 1 (inc (count tokens))))
         (map #(reduce * %))
         (reduce +))))

;(solve "resources/input.txt")