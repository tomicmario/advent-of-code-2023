(ns day.10
  (:require [clojure.string :as str]))

(def tiles {\| #{:n :s}
            \- #{:e :w}
            \L #{:n :e}
            \J #{:n :w}
            \7 #{:s :w}
            \F #{:s :e}
            \. #{}
            \S #{:n :s :e :w}})

(def connector {:n :s 
                :e :w
                :w :e
                :s :n})

(def coordinate-diff {:n [0 -1]
                      :e [1 0]
                      :w [-1 0]
                      :s [0 1]})

(defn bfs [graph start]
  (loop [queue [start :new-depth]
         visited #{start}
         depth 0]
    (if (seq queue)
      (if (= :new-depth (first queue)) (recur (conj (rest queue) :new-depth) visited (inc depth))
          (let [_ (println "todo")]
            (recur queue visited depth)))
      depth)))

(defn index-line [y l]
  (vec (map-indexed (fn [x char] 
                 {:x x :y y :neighbours (tiles char)}) l)))

(defn nodes-to-check [x y neigh-cardinality]
  {:x (+ x (first (coordinate-diff neigh-cardinality))) 
   :y (+ y (last (coordinate-diff neigh-cardinality))) 
   :connect (connector neigh-cardinality)})

(defn connected 
  [{:keys [x y connect]}
   all-nodes]
   (let [node-to-check (get (get all-nodes x) y)] 
     (cond (nil? node-to-check) nil
           (contains? (:neighbours node-to-check) connect) {:x x :y y}
           :else nil)))

(defn connect-node 
  [{:keys [x y neighbours] :as node}
   all-nodes]
  (let [coor-to-check (mapv #(nodes-to-check x y %) neighbours)] 
    (assoc node :neighbours (filterv identity (mapv #(connected % all-nodes) coor-to-check)))))

(defn connect-nodes-line [line all-nodes]
  (mapv #(connect-node % all-nodes) line))

(defn solve [s]
  (let [nodes (->> (str/split-lines (slurp s))
                   (map-indexed index-line)
                   (vec))
        graph (mapv #(connect-nodes-line % nodes) nodes)]
    graph))

(solve "resources/input.txt")