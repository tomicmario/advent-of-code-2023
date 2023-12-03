(ns task.task1
  (:require [clojure.string :as str]))

(def input "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet")

(defn sanitize [s]
  (->> (filterv #(Character/isDigit %) s)
       (mapv #(Character/digit % 10))))

(defn solve [s]
  (->> (slurp s)
       (str/split-lines )
       (mapv sanitize)
       (mapv #(+ (* 10 (first %)) (last %)))
       (reduce +)))

(solve "resources/input.txt")