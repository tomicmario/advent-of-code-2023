(ns day.7
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(def values {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \Q 12 \K 13 \A 14
             \J 13 }) ;part 2

(defn to-count [acc num]
  (if (contains? acc num) (update acc num inc)
      (assoc acc num 1)))

(defn count-comparison [c1 c2]
  (let [comp (compare (second c2) (second c1))]
    (if-not (zero? comp) comp
            (compare (first c2) (first c1)))))

(defn get-joker-count-index [values] 
  (loop [v values
         i 0]
    (if-not (seq v) -1
            (if (= 1 (ffirst v)) i
              (recur (rest v) (inc i))))))

(defn treat-p2 [values]
  (let [index (get-joker-count-index values)]
    (if (neg? index) values
        (let [joker-filtered (vec (concat (subvec values 0 index) (subvec values (inc index) (count values))))
              current-highest (first joker-filtered)]
          (if-not current-highest values 
                  (update-in joker-filtered [0 1] + (second (values index))))))))

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
  (match h
    [[_ 5]] 10
    [[_ 4] [_ 1]] 9
    [[_ 3] [_ 2]] 8
    [[_ 3] [_ 1] [_ 1]] 7
    [[_ 2] [_ 2] [_ 1]]  6
    [[_ 2] [_ 1] [_ 1] [_ 1]] 5
    [[_ 1] [_ 1] [_ 1] [_ 1] [_ 1]] 4
    :rest 3))

(defn better-hand-tie [c1 c2]
  (loop [a (:original c1) b (:original c2)]
    (let [card_a (first a)
          card_b (first b)
          comp (compare (values card_a) (values card_b))]
      (if-not (and (first a) (first b)) -1
              (if-not (zero? comp) comp
                      (recur (rest a) (rest b)))))))

(defn better-hand [c1 c2]
  (let [comp (compare (hand-level (vec (:hand c1))) (hand-level (vec (:hand c2))))]
    (if-not (zero? comp) comp
            (better-hand-tie c1 c2))))

(defn solve [s]
  (let [tokens (str/split-lines (slurp s))]
    (->> (mapv parse tokens)
         (sort better-hand)
         (mapv :score)
         (map #(concat [%1] [%2]) (range 1 (inc (count tokens))))
         (map #(reduce * %))
         (reduce +))))

(solve "resources/input.txt")
