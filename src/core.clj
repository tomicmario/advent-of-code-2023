(ns core
  (:gen-class)
  (:require [day.1 :as d1]
            [day.2 :as d2]
            [day.3 :as d3]
            [day.4 :as d4]
            [day.5 :as d5]))


(defn dispatch [id input]
  (case id
    1 (d1/solve input)
    2 (d2/solve input)
    3 (d3/solve input)
    4 (d4/solve input)
    5 (d5/solve input)
    "task not yet implemented"))

(defn -main
  [task-num input]
  (dispatch task-num input))

