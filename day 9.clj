(require '[clojure.math.combinatorics :as combo])
(def mat [[100000 66  28 60 34 34 3 108]
[66 10000 22 12 91 121 111 71]
[28 22 10000 39 113 130 35 40]
[60 12 39 10000 63 21 57 83]
[34 91 113 63 10000 9 50 60]
[34 121 130 21 9 10000 27 81]
[3 111 35 57 50 27 10000 90]
[108 71 40 83 60 81 90 10000]])

(defn cost-of-jump [start end] (nth (nth mat end) start))

(def cities (vec (range 8)))

(defn cost-of-cycle [cities] (reduce + (map cost-of-jump cities (rest cities))))
(cost-of-cycle cities)

(reduce max 0 (map cost-of-cycle (combo/permutations cities)))
