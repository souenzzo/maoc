(ns aoc2024.main
  (:require [aoc.io :as aoc]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(defn -main
  [& _]
  (aoc/input 2024 1))

(def day1-input
  (aoc/input 2024 1))

(defn parse
  [input]
  (mapv #(mapv parse-long (string/split % #"\s+"))
    input))

(defn day1
  [input]
  (transduce (map abs)
    +
    (apply mapv (fn [& vs]
                  (apply - vs))
      (mapv sort (apply mapv vector input)))))


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
    parse
    day1)
  (aoc/answer 2024 1 1 936063))

