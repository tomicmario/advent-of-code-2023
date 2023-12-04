(ns task.task4
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn to-set [s]
  (println s)
  (let [sanitized (str/trim s)]
    (->> (str/split sanitized #" ")
         (mapv read-string)
         (set))))

(defn treat [s]
  (let [pre-treated (second (str/split s #":"))
        tokens (str/split pre-treated #" +")
        user-numbers (to-set (first tokens))
        result-numbers (to-set (second tokens))
        matches (count (set/intersection user-numbers result-numbers))
        rep (dec matches)]
    (if (neg? matches) 0 (reduce * (repeat rep 2)))))

(defn solve [s]
  (->> (slurp s)
       (str/split-lines)
       (mapv treat)
       (reduce +)))

(solve "resources/input.txt")
