(ns day.4
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn to-set [s]
  (let [sanitized (str/trim s)]
    (->> (str/split sanitized #" +") 
         (mapv read-string)
         (set))))

(defn parse [s]
  (let [pre-treated (second (str/split s #":"))
        tokens (str/split pre-treated #"\|")
        user-numbers (to-set (first tokens))
        result-numbers (to-set (second tokens))]
    {:usr user-numbers :rst result-numbers :mult 1}))

(defn treat 
  [{:keys [usr rst] :as data}]
  (let [matches (count (set/intersection usr rst))]
    (assoc data :matches matches)))

(defn treat-p1
  [{:keys [matches]}]
  (let [rep (dec matches)
        exp2 #(reduce * (repeat % 2))
        res (if (neg? rep) 0 (exp2 rep))]
    res))

(defn treat-line-p2 [data curr-index]
  (let [{:keys [matches mult]} (data curr-index)
        matched (if (pos? matches) 1 0)
        added-cards (* mult matched)]
    (loop [i (inc curr-index)
           acc data]
      (if (or (= i (count data)) (> i (+ curr-index matches))) acc
          (recur (inc i) (update-in acc [i :mult] + added-cards))))))

(defn treat-p2 [data]
  (mapv :mult (loop [i 0
                     acc data]
                (if (= i (count data)) acc
                    (recur (inc i) (treat-line-p2 acc i))))))

(defn solve [s]
  (->> (slurp s)
       (str/split-lines)
       (mapv parse)
       (mapv treat)
       ;(mapv treat-p1)
       (treat-p2)
       (reduce +)))

;(solve "resources/input.txt")