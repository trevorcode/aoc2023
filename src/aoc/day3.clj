(ns aoc.day3
  (:require [clojure.string :as str]))

(def input (slurp "./resources/day3.txt"))

(defn sym? [c]
  (boolean (re-matches #"[!@#$%^&*\-+/=]" (str c))))

(defn update-num [num digit]
  (update num :num clojure.core/str digit))

(defn get-digits [line]
  (let [indexed-line (map-indexed (fn [idx item] [idx item]) line)]
    (:numbers (reduce (fn [{:keys [numbers in-number?] :as acc} [idx elem]]
                        (cond
                          (and (not (Character/isDigit elem))
                               in-number?)
                          (assoc acc :in-number? false)

                          (and (Character/isDigit elem)
                               (not in-number?))
                          (-> acc
                              (update :numbers conj {:col idx :num (str elem)})
                              (assoc :in-number? true))

                          (and (Character/isDigit elem)
                               in-number?)
                          (update-in acc [:numbers (dec (count (:numbers acc)))] update-num elem)

                          :else acc))
                      {:numbers []
                       :in-number? false}
                      indexed-line))))

(defn get-scan-range [{:keys [col row num]}]
  (for [x (range (dec col) (+ 1 col (count num)))
        y (range (dec row) (+ row 2))
        :when (and (>= y 0)
                   (>= x 0))]
    [y x]))

(defn is-in-symbol? [lines [row col]]
  (sym? (get (nth lines row nil) col nil)))

(defn is-part? [lines digit]
  (some (partial is-in-symbol? lines) (get-scan-range digit)))


(def test-input
  "467*.114..
...*......
..35..633.
......-...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn part1 [input]
  (let [lines (str/split-lines input)
        digits (->> (str/split-lines input)
                    (map get-digits)
                    (map-indexed (fn [idx line] (map #(assoc % :row idx) line)))
                    (flatten))]
    (->> (filter (partial is-part? lines) digits)
         (map :num)
         (map parse-long)
         (reduce +))))

(defn is-gear? [lines [row col]]
  (= \* (get (nth lines row nil) col nil)))

(defn near-by-gears [lines digit]
  (map (fn [gear] {gear digit}) (filter (partial is-gear? lines) (get-scan-range digit))))

(defn part2 [input]
  (let [lines (str/split-lines input)
        digits (->> (str/split-lines input)
                    (map get-digits)
                    (map-indexed (fn [idx line] (map #(assoc % :row idx) line)))
                    (flatten))]
    (->> (map (partial near-by-gears lines) digits)
         (flatten)
         (group-by keys)
         (filter #(= (count (val %)) 2))
         (map (fn [[_ [d1 d2]]]
                (* (parse-long (:num (first (vals d1)))) (parse-long (:num (first (vals d2)))))))
         (reduce +))))

(part2 test-input)
(part2 input)