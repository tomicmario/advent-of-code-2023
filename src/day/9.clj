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

(defn find-next-p1 [sequences]
  (->> (mapv last sequences)
       (reverse)
       (reduce +)))

(defn find-next-p2 [sequences]
  (->> (mapv first sequences)
       (reverse)
       (rest)
       (reduce #(- %2 %1))))

(defn line-to-nums [l]
  (->> (str/split l #" ")
       (mapv read-string)))

(defn solve [s]
  (let [inputs (->> (str/split-lines (slurp s))
                    (mapv line-to-nums))
        sequences (mapv generate-sequences inputs)]
    (println "P1:" (reduce + (mapv find-next-p1 sequences)))
    (println "P2:" (reduce + (mapv find-next-p2 sequences)))))

;(solve "resources/input.txt")