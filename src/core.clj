(ns core
  (:gen-class)
  (:require [task.task1 :as t1]
            [task.task2 :as t2]
            [task.task3 :as t3]))


(defn dispatch [id input]
  (case id
    1 (t1/solve input)
    2 (t2/solve input)
    3 (t3/solve input)
    "task not yet implemented"))

(defn -main
  [task-num input]
  (dispatch task-num input))

