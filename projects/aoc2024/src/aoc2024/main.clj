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


(def example-day2
  ["7 6 4 2 1"
   "1 2 7 8 9"
   "9 7 6 2 1"
   "1 3 2 4 5"
   "8 6 4 4 1"
   "1 3 6 7 9"])

(defn parse-day2
  [input]
  (mapv #(mapv parse-long (string/split % #"\s+"))
    input))

(defn remove-nth
  ([nth]
   (keep-indexed (fn [idx el]
                   (when-not (== idx nth)
                     el))))
  ([nth coll]
   (into []
     (remove-nth nth)
     coll)))


(defn safe-line?
  ([x] (safe-line? x 0))
  ([line retry]
   (let [diffs (mapv (fn [[a b]]
                       (- a b))
                 (partition 2 1 line))
         safe? (cond
                 (every? pos? diffs) (every? #(<= 1 (abs %) 3) diffs)
                 (every? neg? diffs) (every? #(<= 1 (abs %) 3) diffs)
                 :else false)]
     (cond
       safe? true
       (pos? retry) (some #(safe-line? % (dec retry))
                      (map-indexed
                        (fn [idx _]
                          (remove-nth idx line))
                        line))
       :else false))))


(comment
  (count (filter safe-line? (parse-day2 example-day2)))
  (count (filter safe-line? (parse-day2 (aoc/input 2024 2))))
  (count (filter #(safe-line? % 1)
           (parse-day2 example-day2)))
  (count (filter #(safe-line? % 1)
           (parse-day2 (aoc/input 2024 2)))))


(def example-day3
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def example-day3-2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn day3
  [input]
  (->> (re-seq #"(mul\([0-9]{1,3},[0-9]{1,3}\)|do\(\)|don't\(\))" input)
    (map first)
    (into [] (fn [rf]
               (let [*enabled? (volatile! true)]
                 (fn
                   ([] (rf))
                   ([el] (rf el))
                   ([acc el]
                    (case (first el)
                      \d (do (case (count el)
                               4 (vreset! *enabled? true)
                               7 (vreset! *enabled? false))
                             acc)
                      (if @*enabled?
                        (rf acc el)
                        acc)))))))
    (map (fn [s] (apply * (map parse-long (re-seq #"[0-9]+" s)))))
    (apply +)))

(comment
  (day3 example-day3)
  (day3 (string/join (aoc/input 2024 3)))
  (day3 example-day3-2))

