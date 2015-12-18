(require '[clojure.math.combinatorics :as combo])

;part 1
(defn gimme-the-answer [raw-input target]
  (let [input (vec raw-input)]
    (->> (range (count input))
         combo/subsets
         (filter (fn [s] (->> (map (partial get input) s)
                              (reduce +)
                              (= target))))
         count)
    )
  )

(->> [20 15 10 5 5]
     combo/subsets
     (filter (comp (partial > 30) (partial reduce +))))

(gimme-the-answer [20 15 10 5 5] 25)

(gimme-the-answer [33 14 18 20 45 35 16 35 1 13 18 13 50 44 48 6 24 41 30 42] 150)

(->> input
     combo/subsets
     (filter #(= (reduce + %) 150))
     count)

(take 10 (combo/subsets [20 15 10 5 5]))

;part 2:
(let [input [33 14 18 20 45 35 16 35 1 13 18 13 50 44 48 6 24 41 30 42]]
  (->> (range (count input))
       combo/subsets
       (filter (fn [s] (->> (map (partial get input) s)
                            (reduce +)
                            (= 150))))
       (map count)
       (reduce min)
       )
  )
; answer!
(let [input [33 14 18 20 45 35 16 35 1 13 18 13 50 44 48 6 24 41 30 42]]
  (as-> (range (count input)) foo
        (combo/combinations foo 4)
        (filter (fn [s] (->> (map (partial get input) s)
                             (reduce +)
                             (= 150)))
                foo)
       (count foo)
       )
  )