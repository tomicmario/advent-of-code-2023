(ns day.6
  (:require [clojure.string :as str]))

(defn parse-p1 [s]
  (mapv read-string (-> (second (str/split s #":"))
                        (str/trim)
                        (str/split #" +"))))

(defn parse-p2 [s]
  (-> (second (str/split s #":"))
      (str/trim)
      (str/replace #" +" "")))

(defn distances [[time dist]]
  (loop [held 0
         acc []]
    (if (= held time) acc
        (let [travelled (* held (- time held))
              updated (if (> travelled dist) (conj acc travelled) acc)]
          (recur (inc held) updated)))))

(defn solve [s]
  (let [tokens (str/split-lines (slurp s))
        time (parse (first tokens))
        distance (parse (second tokens))
        data (map #(concat [%1] [%2]) time distance)]
    (->> (mapv distances data)
         (mapv count)
         (reduce *))))

(solve "resources/input.txt")