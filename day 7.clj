; wheee!
(require '[clojure.string :as s])

(def get-value
  (memoize
   (fn [circuit register]
     (let [definition (get circuit register)]
       (if (nil? definition)
         register
         (apply (first definition)
                (map (partial get-value circuit)
                     (rest definition))
                )
         )
       )
     )
   )
  )

(Integer/toHexString 2r1111111111111111)
(Integer/toBinaryString (bit-xor 0xFFFF 123))

(def b-not (partial bit-xor 0xFFFF))
(defn b-shift-left [x bits] (bit-and 0xFFFF (bit-shift-left x bits)))

(def simple-circuit {:x [identity 123]
                     :y [identity 456]
                     :d [bit-and :x :y]
                     :e [bit-or :x :y]
                     :f [b-shift-left :x 2]
                     :g [bit-shift-right :y 2]
                     :h [b-not :x]
                     :i [b-not :y]})

(re-matches #"\d+" "12a")
(defn parse-val [input] (if (re-matches #"\d+" input) (Integer/parseInt input) (keyword input)))
(def ops {:NOT b-not, :AND bit-and, :OR bit-or, :RSHIFT bit-shift-right, :LSHIFT b-shift-left})
(defn munge-line [output raw]
  (let [line (s/split raw #" ")
        the-key (keyword (last line))
        terms (map parse-val (drop-last 2 line))
        the-rule (concat (if (= 1 (count terms))
                           [identity]
                           (map ops (filter ops terms)))
                         (remove ops terms))]
    (assoc output the-key the-rule)
    ))

(def s-c-2 (reduce munge-line {} (s/split-lines "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i")))

(count big-input)

(def big-circuit (reduce munge-line {} big-input))
(count big-circuit)
(big-circuit :a)

(get-value big-circuit :a)

(def big-circuit-part-two (assoc big-circuit :b [identity 16076]))

(get-value big-circuit-part-two :a)