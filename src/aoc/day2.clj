(ns aoc.day2
  (:require [clojure.string :as str]))

(def input (slurp "./resources/day2.txt"))

(defn setstext->set [set-text]
  (into {} (map (fn [t]
                  (let [s (str/split t #" ")]
                    {(second s) (parse-long (first s))}))
                (str/split set-text #", "))))

(defn line->game [line]
  (let [game-id (parse-long (last (str/split (first (str/split line #":")) #" ")))
        sets-text (map str/trim (str/split (second (str/split line #":")) #";"))
        sets (map setstext->set sets-text)]
    {game-id sets}))

(defn game-possible? [game]
  (and (<= (get game "green" 0) 13)
       (<= (get game "blue" 0) 14)
       (<= (get game "red" 0) 12)))

(defn set-possible? [set]
  (every? game-possible? (val set)))

(defn part1 []
  (->> (str/split-lines input)
       (into {} (map line->game))
       (filter set-possible?)
       (map key)
       (reduce +)))

(defn min-blocks-game [game]
  (reduce #(merge-with max %1 %2) {} game))

(defn game-power [game]
  (apply * (vals game)))

(defn part2 []
  (->> (str/split-lines input)
       (into {} (map line->game))
       (vals)
       (map min-blocks-game)
       (map game-power)
       (reduce +)))

(part1)
(part2)
