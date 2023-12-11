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
         visited #{}
         depth 0]
    ;(println queue visited depth)
    (cond 
      (every? #(= % :new-depth) queue) depth
      (= :new-depth (first queue)) (recur (concat (rest queue) [:new-depth]) visited (inc depth)) 
      (visited (first queue)) (recur (rest queue) visited depth) 
      :else (let [current (first queue)
                  node (get (get graph (:y current)) (:x current))
                  neigh-not (remove #(visited %) (:neighbours node))
                  updated-queue (concat (rest queue) neigh-not)
                  visited-updated (conj visited {:x (:x node) :y (:y node)})]
              (recur updated-queue visited-updated depth)))))

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
   (let [line (get all-nodes y)
         char (get line x)] 
     (cond (nil? char) nil
           (contains? (:neighbours char) connect) {:x x :y y}
           :else nil)))

(defn connect-node 
  [{:keys [x y neighbours] :as node}
   all-nodes]
  (let [coor-to-check (mapv #(nodes-to-check x y %) neighbours)] 
    (assoc node :neighbours (filterv identity (mapv #(connected % all-nodes) coor-to-check)))))

(defn connect-nodes-line [line all-nodes]
  (mapv #(connect-node % all-nodes) line))

(defn extract-start [graph [x y]]
  (let [line (get graph y)
        tile (get line x)]
    tile))

(defn solve [s]
  (let [str-input (str/split-lines (slurp s))
        nodes (->> str-input
                   (map-indexed index-line)
                   (vec))
        graph (mapv #(connect-nodes-line % nodes) nodes)
        start (->> (map-indexed (fn [idx s] [idx (.indexOf s "S")]) str-input)
                         (remove #(neg? (second %)))
                         (flatten)
                         (extract-start graph))]
    (bfs graph start)))

(solve "resources/input.txt")