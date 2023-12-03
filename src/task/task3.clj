(ns task.task3
  (:require [clojure.string :as str]))

(def input "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..")

(def symbol-regex #"\D") 

(defn sanitize [s]
  (-> (str/replace s symbol-regex #(str "." %))
      (str/split #"\.+")))

(defn tokenize [s]
  (let [tokens (->> (sanitize s)
                    (filterv not-empty))]
    {:line s :tokens tokens}))

(defn treat-symbol [tokens line curr-index acc]
  (let [index (str/index-of line (first tokens) curr-index)
        updated-acc (update acc :symbols conj index)]
    {:tokens (rest tokens) :current-index index :acc updated-acc}))

(defn treat-number [tokens line curr-index acc]
  (let [current (first tokens) 
        value (read-string current)
        index (str/index-of line current curr-index)
        range {:val value :min (dec index) :max (+ 1 index (count current))}
        updated-acc (update acc :numbers conj range)]
    {:tokens (rest tokens) :current-index index :acc updated-acc}))

(defn treat [tokens line curr acc]
  (if (re-matches symbol-regex (first tokens)) 
    (treat-symbol tokens line curr acc)
    (treat-number tokens line curr acc)))

(defn gen-range [data]
 (let [line (:line data)]
  (loop [tokens (:tokens data) current-index 0 
         acc {:numbers [] :symbols []}]
    (if (empty? tokens) (merge data acc)
      (let [result (treat tokens line current-index acc)]
        (recur (:tokens result) (:current-index result) (:acc result)))))))

(defn add-up-down [data]
  (loop [d data 
         index 0]
    (if (< index (count data))
      (let [up (or (get d (dec index)) {:symbols []})
            down (or (get d (inc index)) {:symbols []})
            current (:symbols (d index))
            total (concat current (:symbols up) (:symbols down))]
        (recur (assoc-in d [index :symbols-all] total) (inc index))) 
      d)))

(defn in-range?
  [{:keys [min max]}
   index]
  (<= min index max))

(defn verify [number symbols] 
  (if (some #(in-range? number %) symbols) number 
      {:val :wrong}))

(defn keep-valid 
  [{:keys [numbers symbols-all]}]
  (->> (mapv #(verify % symbols-all) numbers)
       (mapv :val)
       (filterv number?)))

(defn solve [s]
  (->> (str/split-lines s) 
       (mapv tokenize)
       (mapv gen-range)
       (mapv #(select-keys % [:numbers :symbols]))
       (add-up-down)
       (mapv keep-valid)
       (flatten)
       (reduce +)))

(solve input)