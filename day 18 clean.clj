(ns day-18
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

(def test-input-string ".#.#.#
...##.
#....#
..#...
#.#..#
####..")

(defn promote [y row]
  (map (fn [[coords value]] [(conj coords y) value]) row))

(defn convert-to-map [table]
  (if (coll? table)
    (->> table
         (map convert-to-map)
         (map-indexed promote)
         (reduce into {}))
    {[] table}
    )
  )

(defn parse-grid-from-string [grid-as-string]
  (->> grid-as-string
       s/split-lines
       (map seq)
       convert-to-map
       (map (fn [[coords value]] [coords (= value \#)]))
       (into {})))

(defn neighbors [coords]
  (->> (map #(range (dec %) (inc (inc %))) coords)
       (apply combo/cartesian-product)
       (map vec)
       (remove (partial = coords)))
  )

(defn step-cell [grid [coords value]]
  (let [live-neighbors (->> (neighbors coords)
                            (map (partial get grid))
                            (filter identity)
                            count)]
    [coords
     (if value
       (or (= live-neighbors 2) (= live-neighbors 3))
       (= live-neighbors 3))]
    )
  )

(defn step-grid [current-grid]
  (->> current-grid
       (map (partial step-cell current-grid))
       (into {}))
  )

(def test-life-part-1 (iterate step-grid
                               (parse-grid-from-string test-input-string)))

;test part 1 answer, should be 4
(->> (nth test-life-part-1 4)
     vals
     (filter identity)
     count)

;puzzle input
;(def puzzle-input "") ;paste puzzle input in here

;part 1 answer:
(->> (nth (iterate step-grid
                   (parse-grid-from-string puzzle-input)) 100)
     vals
     (filter identity)
     count)

;part 2

;test part 2 answer, should be 17
(let [stuck-lights (zipmap [[0 0] [0 5] [5 0] [5 5]] (repeat true))]
  (->> (nth (iterate (comp #(merge % stuck-lights) step-grid)
                     (merge (parse-grid-from-string test-input-string) stuck-lights)
                     )
            5)
       vals
       (filter identity)
       count))

;part 2 answer:
(let [stuck-lights (zipmap [[0 0] [0 99] [99 0] [99 99]] (repeat true))]
  (->> (nth (iterate (comp #(merge % stuck-lights) step-grid)
                     (merge (parse-grid-from-string puzzle-input) stuck-lights)
                     )
            100)
       vals
       (filter identity)
       count))
