(ns task.task2
  (:require [clojure.string :as str]))

(def valid {:red 12 :green 13 :blue 14})
(def balls-count (into {} (for [[k] valid] [k 0])))

(defn color-to-key [s]
  (let [kw (keyword s)]
    (if (kw valid) kw
      (throw (Exception. "Color not supported")))))

(defn to-colour-count [s]
  ; Expected : "[number] [color]"
  (let [args (str/split (str/trim s) #" ")
        color (color-to-key (second args))
        count (Integer/parseInt (first args))]
    {:colour color :colour-count count}))

(defn treat-draft [s]
  (loop [c (mapv to-colour-count (str/split s #"[,]"))
         acc balls-count]
    (let [x (first c)]
      (if (nil? x) acc
          (recur (rest c) (update acc (:colour x)  + (:colour-count x)))))))

(defn treat-balls [s]
  ; Expected "[draft1];[draft2]..."
  ; with draft as "[color1draft],[color2draft]..."
  (let [tokens (str/split s #"[;]")]
    (mapv treat-draft tokens)))

(defn input-to-map [s]
  ; Expected: [game n°]:[drafts]
  (let [tokens (str/split s #":")
        game-id (str/replace (first tokens) #"game " "")
        balls (treat-balls (last tokens))]
  {:balls balls :id (read-string game-id)}))

(defn max-balls [left right]
  (into {} (for [[k, v] left] {k (Math/max v (k right))})))

(defn maximum
  ; ugly
  [{:keys [balls id]}]
    {:id id :balls
     (reduce max-balls balls)})

(defn answer-part-1 [data]
  (->> data 
       (filter #(every? true? (for [[k v] valid] (<= (k (:balls %)) v))))
       (mapv :id)
       (reduce +)))

(defn answer-part-2 [data]
  (->> (mapv #(vec (for [[_ v] (:balls %)] v)) data)
       (mapv #(reduce * %))
       (reduce +)))

(defn solve [s]
  (->> (slurp s)
       (str/lower-case)
       (str/split-lines)
       (mapv input-to-map)
       (mapv maximum)
       ;(answer-part-1)
       (answer-part-2)
       ))

;(solve "resources/input.txt")