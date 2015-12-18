(require '[clojure.math.combinatorics :as combo])

;part 1
(defn satisfactory-eggnog-divisions [input target]
  (->> (range (count input))
       combo/subsets
       (filter #(->> (map (partial get input) %)
                     (reduce +)
                     (= target)))))

(def thems-the-nogs (satisfactory-eggnog-divisions
                     [33 14 18 20 45 35 16 35 1 13 18 13 50 44 48 6 24 41 30 42]
                     150))
(count thems-the-nogs)

;part 2:
(def smallest-division-size (reduce min (map count thems-the-nogs)))

(count (filter #(= (count %) smallest-division-size) thems-the-nogs))
