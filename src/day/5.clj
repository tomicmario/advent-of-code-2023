(ns day.5
  (:require [clojure.string :as str]))

(defn arr-to-range [[dest src range]]
  {:dest dest :src src :range range})

(defn parse-line [s]
  (->> (str/split s #" ")
       (mapv read-string)))

(defn parse [s]
  (let [line-input (rest (str/split-lines s))]
    (->> (mapv parse-line line-input)
         (mapv arr-to-range))))

(defn in-a-range? [i {:keys [src range]}]
  (<= src i (+ src range -1)))

(defn apply-range [i {:keys [dest src]}]
  (+ dest (- i src)))

(defn traverse [i data]
  (let [ranges (filterv #(in-a-range? i %) data)]
    (if (seq ranges) (apply-range i (first ranges)) i)))

(defn reach-target [i data]
  (->> (concat [i] data)
       (reduce traverse)))

(defn solve [s]
  (let [tokens (str/split (slurp s) #"\n\n")
        data (mapv parse (rest tokens))
        input (mapv read-string (-> (first tokens)
                                    (str/replace #"seeds: " "")
                                    (str/split #" ")))]
    (->> (mapv #(reach-target % data) input)
         (reduce #(if (< %1 %2) %1 %2)))))

;(solve "resources/input.txt")