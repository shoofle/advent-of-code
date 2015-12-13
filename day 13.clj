(require '[clojure.string :as s])
(require '[clojure.math.combinatorics :as combo])

(defn make-happinesses [corpus]
  (reduce (fn [happinesses line]
            (let [pieces (s/split line #" ")

                  a (nth pieces 0)
                  quantity (read-string (nth pieces 3))
                  adjustment (if (= "gain" (nth pieces 2)) quantity (- quantity))
                  b ; gotta remove the period at the end!
                  (.substring (last pieces) 0 (dec (count (last pieces))))]
              (assoc happinesses a
                (assoc (get happinesses a {}) b adjustment))
              ))
          {}
          (s/split-lines corpus)
          ))

(def test-matrix (make-happinesses "Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol."))
(get test-matrix "Alice")
(defn total-haps [happinesses seating]
  (reduce + 0
          (map (fn [a b] (+ (get (get happinesses a) b)
                            (get (get happinesses b) a)))
               seating
               (next (cycle seating))))
  )
(total-haps test-matrix ["Alice" "Bob" "Carol" "David"])

;paste the input into here
;(def actual-matrix (make-happinesses ""))

;part 1!
(reduce max
        (map (partial total-haps actual-matrix)
             (combo/permutations (keys actual-matrix))))
;it's 664

;part 2: add me, and all seatings that involve me result in an adjustment of zero
(def people-at-the-party (keys actual-matrix))

(def part-2-matrix
  "New happinesses, including me this time."
  (assoc
    (reduce (fn [acc [person likes]]
              (assoc acc person (assoc likes "Reader" 0)))
            {}
            actual-matrix)
    "Reader" (zipmap people-at-the-party (repeat 0))))

(reduce max
        (map (partial total-haps part-2-matrix)
             (combo/permutations (keys part-2-matrix))))
;it's 640

;oh my god i ruined the party