(ns day.8
   (:require [clojure.string :as str]))

(defn traverse [input nodes]
  (loop [i 1 inp input cur-node "AAA"]
    (let [cur (first inp)]
      (if-not cur (recur i input cur-node)
              (let [node (get (get nodes cur-node) cur)] 
               (if (= node "ZZZ") i
                  (recur (inc i) (rest inp) node )))))))

(defn parse-node [s]
  (let [tokens (str/split s #" = ")
        left-right (-> (second tokens)
                       (str/replace #"[()]" "")
                       (str/split  #", "))]
    {(first tokens) {\L (first left-right ) \R (second left-right)}}))

(defn solve [s]
  (let [tokens (str/split (slurp s) #"\n\n")
        input (mapv identity (first tokens))
        nodes (into {} (map parse-node (str/split-lines (second tokens))))]
    (traverse input nodes)))

(solve "resources/input.txt")