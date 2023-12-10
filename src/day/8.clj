(ns day.8
   (:require [clojure.string :as str] 
             [clojure.math.numeric-tower :as math]))

(defn traverse [input nodes start end-fn]
  (loop [i 1 inp input cur-node start]
    (let [cur (first inp)]
      (if-not cur (recur i input cur-node)
              (let [node (get (get nodes cur-node) cur)] 
               (if (end-fn node) i
                  (recur (inc i) (rest inp) node )))))))

(defn parse-node [s]
  (let [tokens (str/split s #" = ")
        left-right (-> (second tokens)
                       (str/replace #"[()]" "")
                       (str/split  #", "))]
    {(first tokens) {\L (first left-right ) \R (second left-right)}}))

(defn p2 [input nodes]
  (let [end-fn #(= \Z (last %))]
    (->> (mapv #(traverse input nodes % end-fn)
               (filter #(= \A (last %)) (map key nodes)))
         (reduce math/lcm))))

(defn solve [s]
  (let [tokens (str/split (slurp s) #"\n\n")
        input (mapv identity (first tokens))
        nodes (into {} (map parse-node (str/split-lines (second tokens))))]
    ;(traverse input nodes "AAA" #(= % "ZZZ")) ;p1
    (p2 input nodes)))

;(solve "resources/input.txt")