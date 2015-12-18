(require '[clojure.string :as s])
(require '[clojure.math.combinatorics :as combo])
(def grid-vecs-chars (map vec (s/split-lines grid-strings)))
(def grid
  (into {} (reduce concat
  (map-indexed (fn [y row]
                 (map-indexed (fn [x value]
                                [[x y]
                                 (if (= value \#) :on :off)])
                              row))
               grid-vecs-chars))))

(range (dec 0) (inc (inc 0)))

(defn neighbors [[x y]]
  (->> (combo/cartesian-product (range (dec x) (inc (inc x)))
                                (range (dec y) (inc (inc y))))
       (map vec)
       (remove (partial = [x y]))
       (filter (partial get grid)))
  )
(neighbors [0 0])

(map #{:on} [:on :on :off :off])
(defn step-cell [grid [x y] value]
  (let [neighbors-value (->> (neighbors [x y])
                             (map (partial get grid))
                             (filter #{:on})
                             count)]
    (if (= value :on)
      (if (or (= neighbors-value 2)
              (= neighbors-value 3))
        :on
        :off)
      (if (= neighbors-value 3)
        :on
        :off))
    )
  )

(defn step-grid [grid]
  (->> grid
       (map (fn [[coords value]] [coords (step-cell grid coords value)]))
       (into {}))
  )

grid
(step-grid grid)

(def life (iterate step-grid grid))

(nth life 101)
(count (filter (fn [[coords value]] (= value :on)) (nth life 100)))


(defn step-grid-2 [grid]
  (merge (->> grid
              (map (fn [[coords value]] [coords (step-cell grid coords value)]))
              (into {}))
         {[0 0] :on
          [0 99] :on
          [99 0] :on
          [99 99] :on}
         ))
(def part-2 (iterate step-grid-2
                     (merge grid
                            {[0 0] :on
                             [0 99] :on
                             [99 0] :on
                             [99 99] :on})))

(count (filter (fn [[coords value]] (= value :on)) (nth part-2 100)))