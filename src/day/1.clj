(ns day.1
  (:require [clojure.string :as str]))

(def spelt {"one" "o1ne" "two" "t2wo" "three" "th3ree" "four" "fo4ur" "five" "fi5ve" "six" "si6x" "seven" "se7ven" "eight" "ei8ght" "nine" "ni9ne"})

(defn replace-written [all]
  (loop [s all
         values (seq spelt)] 
    (if (seq values)
      (let [curr (first values)] 
        (recur (str/replace s (java.util.regex.Pattern/compile (first curr)) (last curr))
               (rest values)))
      s)))

(defn sanitize [s]
  (->> (filterv #(Character/isDigit %) s)
       (mapv #(Character/digit % 10))))

(defn solve [s]
  (->> (slurp s)
       (replace-written)
       (str/split-lines)
       (mapv sanitize)
       (mapv #(+ (* 10 (first %)) (last %)))
       (reduce +)))

;(solve "resources/input.txt")