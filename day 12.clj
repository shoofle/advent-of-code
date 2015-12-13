;(def inp-obj ) strip out all colons in the input file and paste it in here

(str inp-obj)

; part 1!
(reduce + (map read-string
               (re-seq #"-?\d+"
                       (str inp-obj))))

(type inp-obj)
(char 91)

(#{"red"} "red")
(defn clean-reds [input]
  "removes every object that contains any key with a value of 'red'"
  (cond (map? input)
        (if (some #{"red"} (vals input))
          nil
          (into {} (map (fn [[k v]] [k (clean-reds v)]) input))
          )

        (vector? input)
        (mapv clean-reds input)

        :else
        input
        )
  )

(defn sum-up [input]
  (cond (map? input)
        (reduce + 0 (map (fn [[k v]] (+ (sum-up k) (sum-up v))) input))

        (vector? input)
        (reduce + 0 (map sum-up input))

        (number? input) input

        :else 0
        )
  )

;part 2!
(sum-up (clean-reds inp-obj))

(into {} (map (juxt first (comp clean-reds second)) {"a" 12} ))

(clean-reds inp-obj)
(re-seq #"-?\d+"
        (str (clean-reds inp-obj)))

(reduce + (map read-string
               (re-seq #"-?\d+"
                       (str (clean-reds [1,"red",5])))))

(defn one-step [input]
  (cond (map? input)
        (if (some #{"red"} (vals input))
          0
          (reduce + (map one-step (vals input))))

        (or (vector? input) (seq? input))
        (reduce + (map one-step input))

        (number? input) input
        :else 0
        ))

(one-step inp-obj)
