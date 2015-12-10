; day six
; turn on 0,0 through 999,999 would turn on (or leave on) every light.
; toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
; turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.
(require '[clojure.string :as str])

(defn to-i [x y] (+ (* 1000 y) x))
(defn range-to-i [[start-x start-y] [end-x end-y]]
  (mapcat #(map to-i (range start-x (inc end-x)) (repeat %))
          (range start-y (inc end-y)))
  )

(range-to-i [499 499] [500 500])

(defn rule-applications [rule start end]
  (zipmap (range-to-i start end)
          (repeat rule))
  )

(rule-applications "turn off" [499 499] [500 500])

(def lights (vec (repeat 1000000 ["off"])))

(defn do-rule [lights the-range rule]
  (reduce (fn [ongoing idx] (assoc ongoing idx (conj (nth ongoing idx) rule)))
          lights
          the-range)
  )

(defn munge-line
  "returns [rule [startx starty] [endx endy]]"
  [input]
  (let [splits-backwards (reverse (str/split input #" "))
        start-comma (nth splits-backwards 2)
        end-comma (nth splits-backwards 0)
        rule (cond (= (last splits-backwards) "toggle") "toggle"
                   (= (nth splits-backwards 3) "on") "on"
                   (= (nth splits-backwards 3) "off") "off"
                   :else nil)
        ]
    [rule
     (map read-string (str/split start-comma #","))
     (map read-string (str/split end-comma #","))]
    )
  )

(defn munge-input
  "returns a list of [[startx starty] [endx endy] rule]"
  [input]
  (map munge-line (str/split input #"\n")))

(def test1 "turn on 0,0 through 999,999
turn on 1,1 through 998,998")
(munge-input test1)

(def testr (reduce (fn [ongoing [rule start end]]
          (do-rule ongoing (range-to-i start end) rule))
        lights
        (munge-input test1)))

(def output (reduce (fn [ongoing [rule start end]]
                      (do-rule ongoing (range-to-i start end) rule))
                    lights
                    (munge-input input)))

(defn find-result [list-of-rules]
  (reduce (fn [state rule]
            (case rule
              "on" "on"
              "off" "off"
              "toggle" (if (= state "on") "off" "on")))
          "off"
          list-of-rules))

(def results (map find-result output))

(def answer-1 (count (filter #(= % "on") results)))

(defn find-result-santish [list-of-rules]
  (reduce (fn [brightness rule]
            (case rule
              "on" (inc brightness)
              "off" (max 0 (dec brightness))
              "toggle" (+ 2 brightness)))
          0
          list-of-rules))

(def results-2 (map find-result-santish output))
(def answer-2 (reduce + results-2))
answer-2
