(ns task.task2
  (:require [clojure.string :as str]))

(def input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(def valid {:red 12 :green 13 :blue 14})
(def balls-count {:red 0 :green 0 :blue 0} )

(defn color-to-key [s]
  (let [keyword (keyword s)]
    (if (keyword balls-count) keyword
      (throw (Exception. "Color not supported")))))

(defn colour-count [s]
  (let [args (str/split (str/trim s) #" ")]
    [(color-to-key (last args)) (Integer/parseInt (first args))]))

(defn treat-balls [s]
  (let [tokens (str/split s #"[,;]")
        updated-tokens (mapv colour-count tokens)]
    (loop [x updated-tokens 
           acc balls-count]
       (if (seq x)
         (recur (rest x) (update acc (first (first x)) + (last (first x))))
         acc))))

(defn input-to-map [s]
  (let [tokens (str/split s #":")
        game-id (str/replace (first tokens) #"game " "")
        balls (treat-balls (last tokens))]
  (assoc balls :id (read-string game-id))))

(defn valid?
  [{:keys [red green blue]}]
  (and (<= red (:red valid)) (<= green (:green valid)) (<= blue (:blue valid))))

(defn solve [s]
  (->> (str/lower-case s)
       (str/split-lines)
       (mapv input-to-map)
       (filterv valid?)
       (mapv :id)
       (reduce +)))

(solve input)