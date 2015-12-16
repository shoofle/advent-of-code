(require '[clojure.string :as s])

(def input "Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.
Cupid can fly 8 km/s for 17 seconds, but then must rest for 114 seconds.
Prancer can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
Donner can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.
Dasher can fly 11 km/s for 12 seconds, but then must rest for 125 seconds.
Comet can fly 21 km/s for 6 seconds, but then must rest for 121 seconds.
Blitzen can fly 18 km/s for 3 seconds, but then must rest for 50 seconds.
Vixen can fly 20 km/s for 4 seconds, but then must rest for 75 seconds.
Dancer can fly 7 km/s for 20 seconds, but then must rest for 119 seconds.")

(defn make-reindeer [n speed seconds resting]
  [n (cycle (concat (repeat seconds speed) (repeat resting 0)))])

(def reindeer
  (let [lines (map #(s/split % #" ") (s/split-lines input))
        arguments (map (fn [line] [(first line) ;name
                                   (read-string (nth line 3)) ;speed
                                   (read-string (nth line 6)) ;sustain time
                                   (read-string (nth line 13)) ;rest time
                                   ])
                       lines)
        pairs (map (partial apply make-reindeer) arguments)
        results (into {} pairs)]
    results
    )
  )

(def race-results (into {}
                        (map (fn [[k v]] [k (reduce + 0 (take 2503 v))])
                             reindeer)))

race-results
;part 1!
(reduce max (map second race-results))


(def anon-reindeer (map second reindeer))

(def all-the-reins (apply map list anon-reindeer))
(defn race-step [[distance points] speeds]
  (let [new-distance (map + distance speeds)
        leader (reduce max new-distance)
        new-points (map #(if (= leader %1) (inc %2) %2) new-distance points)
        ]
    [new-distance new-points]
    )
  )

(def p2-race-results (reduce race-step [(repeat 0) (repeat 0)] (take 2503 all-the-reins)))

;part 2!
(reduce max (second p2-race-results))




