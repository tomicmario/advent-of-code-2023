(ns day.9
  (:require [clojure.string :as str]))

(defn next-line [l]
  (->> (map vector (butlast l) (rest l))
       (map #(reduce - %))
       (mapv -)))

(defn generate-sequences [history]
  (loop [acc [history]]
    (if (every? zero? (last acc)) acc
        (recur (conj acc (next-line (last acc)))))))

(defn find-next [history]
  (let [sequences (generate-sequences history)]
    (->> (mapv last sequences)
         (reverse)
         (reduce +))))

(defn line-to-nums [l]
  (->> (str/split l #" ")
       (mapv read-string)))

(defn solve [s]
  (let [inputs (->> (str/split-lines (slurp s))
                    (mapv line-to-nums))]
    (->> (mapv find-next inputs)
         (reduce +))))

(solve "resources/input.txt")