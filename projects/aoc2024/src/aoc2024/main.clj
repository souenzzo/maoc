(ns aoc2024.main
  (:require [aoc.io :as aoc]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(defn -main
  [& _]
  (aoc/input 2024 1))

(def day1-input
  (aoc/input 2024 1))

(defn parse-day1
  [input]
  (mapv #(mapv parse-long (string/split % #"\s+"))
    input))

(defn day1-answer1
  [input]
  (transduce (map abs)
    +
    (apply mapv (fn [& vs]
                  (apply - vs))
      (mapv sort (apply mapv vector input)))))

(defn day1-answer2
  [input]
  (let [[left right] (apply mapv vector input)]
    (apply + (for [el left]
               (* el (count (filter #{el} right)))))))

(comment
  (-main)
  (def example-day-01
    ["3   4"
     "4   3"
     "2   5"
     "1   3"
     "3   9"
     "3   3"])
  (-> day1-input
    parse-day1
    day1-answer1)
  (-> day1-input
    parse-day1
    day1-answer2)
  (aoc/answer 2024 1 1 936063)
  (aoc/answer 2024 1 2 23150395))

