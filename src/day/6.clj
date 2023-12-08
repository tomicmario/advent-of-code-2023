(ns day.6
  (:require [clojure.string :as str]))

(defn parse-p1 [s]
  (mapv read-string (-> (second (str/split s #":"))
                        (str/trim)
                        (str/split #" +"))))

(defn parse-p2 [s]
  (-> (second (str/split s #":"))
      (str/trim)
      (str/replace #" +" "")
      (read-string)))

(defn distances [[time dist]]
  (loop [held 0
         acc []]
    (if (= held time) acc
        (let [travelled (* held (- time held))
              updated (if (> travelled dist) (conj acc travelled) acc)]
          (recur (inc held) updated)))))

(defn data-p1 [tokens]
  (let [time (parse-p1 (first tokens))
        distance (parse-p1 (second tokens))]
    (map #(concat [%1] [%2]) time distance)))

(defn data-p2 [tokens]
  (let [time (parse-p2 (first tokens))
        distance (parse-p2 (second tokens))]
    [[time distance]]))

(defn solve [s]
  (let [tokens (str/split-lines (slurp s))
        data (data-p2 tokens)]
    (->> (mapv distances data)
         (mapv count)
         (reduce *))))

;(solve "resources/input.txt")