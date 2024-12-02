(ns aoc2023.main
  (:require [aoc.io :as aoc]
            [clojure.string :as string]))

(def day1-input
  (aoc/input 2023 1))

(defn -main
  [& _])

(def example-1
  ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"])

(def example-2
  ["two1nine" "eightwothree" "abcone2threexyz" "xtwone3four" "4nineeightseven2" "zoneight234" "7pqrstsixteen"])

(defn find-numbers
  [s]
  (let [[match] (re-find
                  #"([0-9](.{0,}[0-9])?)"
                  (-> s
                    (string/replace #"nine" "9")
                    (string/replace #"eight" "8")
                    (string/replace #"seven" "7")
                    (string/replace #"six" "6")
                    (string/replace #"five" "5")
                    (string/replace #"four" "4")
                    (string/replace #"three" "3")
                    (string/replace #"two" "2")
                    (string/replace #"one" "1")))]
    (parse-long (str (first match)
                  (last match)))))

(comment
  (-main)
  (apply + (map find-numbers day1-input))
  (apply + (map find-numbers example-2)))