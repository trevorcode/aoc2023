(ns aoc.day1
  (:require [clojure.string :as str]))


(def day1input (slurp "./resources/day1.txt"))

(defn part1 []
  (->> (str/split-lines day1input)
       (map #(re-seq #"\d" %))
       (map #(Integer/parseInt (str (first %) (last %))))
       (reduce +)))

(defn mapNumberWord [word]
  (or ({"zero" 0 "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9} word) word))

(defn part2 []
  (->> (str/split-lines day1input)
       (map #(map mapNumberWord (map second (re-seq #"(?=(one|two|three|four|five|six|seven|eight|nine|\d))" %))))
       (map #(Integer/parseInt (str (first %) (last %))))
       (reduce +)))

(part1) 
(part2) 