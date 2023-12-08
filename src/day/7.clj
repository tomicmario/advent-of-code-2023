(ns day.7
  (:require [clojure.string :as str]))

(def values {\1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14})

(defn to-count [acc num]
  (if (contains? acc num) (update acc num inc)
      (assoc acc num 1)))

(defn count-comparison [c1 c2]
  (let [comp (compare (second c2) (second c1))]
    (if-not (zero? comp) comp
            (compare (first c2) (first c1)))))

(defn treat-hand [h] 
  (->> (mapv values h)
       (reduce to-count {})
       (sort count-comparison)))

(defn parse [s]
  (let [tokens (str/split s #" ")
        hand-token (first tokens)
        score-token (read-string (last tokens))]
    {:hand (treat-hand hand-token) :score score-token }))


(defn better-hand [c1 c2]
  (loop [a (:hand c1) b (:hand c2)]
    (let [[num1 count1] (first a)
          [num2 count2] (first b)
          comp-num (compare num1 num2)
          comp-count (compare count1 count2)]
      (if-not (and (first a) (first b)) (compare (count b) (count a))
              (if-not (zero? comp-count) comp-count
                      (if-not (and (zero? comp-num) (pos? (compare (last (rest b)) (last ( rest a))))) comp-num
                              (recur (rest a) (rest b))))))))

(defn solve [s]
  (let [tokens (str/split-lines (slurp s))]
    (->> (mapv parse tokens)
         (sort better-hand)
         (mapv :score)
         (map #(concat [%1] [%2]) (range 1 (inc (count tokens))))
         (map #(reduce * %))
         (reduce +))))

(solve "resources/input.txt")