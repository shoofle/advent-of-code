(def break-it-up (comp reverse (comp (partial reduce
                                (fn [acc next-char]
                                  (if (= (first (first acc)) next-char)
                                    (conj (pop acc) (conj (first acc) next-char))
                                    (conj acc (list next-char))
                                    ))
                                '())
                       )))

(break-it-up "1121")

(def build-it-down (partial mapcat #(str (count %) (first %))))

(build-it-down (break-it-up "1121"))

(def day-10-problem (iterate (comp build-it-down break-it-up) "1113222113"))

; part 1
(count (nth day-10-problem 40))

; part 2
(count (nth day-10-problem 50))

