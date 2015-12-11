(def raw-alphabet (map char (range 97 123)))
(def alph (remove #{\i \o \l} raw-alphabet))

(def make-step-table
  "a map from characters in the alphabet to the next character in the alphabet"
  (memoize (fn [alphabet] (zipmap alphabet (rest alphabet)))))
(defn get-next-char [alphabet start] (get (make-step-table alphabet) start))

(defn next-string [alphabet start]
  (if (empty? start) nil
    (let [beginning (butlast start)
          end (last start)
          next-end (get-next-char alphabet end)
          next-beginning (next-string alphabet beginning)]
      (cond next-end (str (apply str beginning) next-end)
            next-beginning (str next-beginning (first alphabet))
            :else nil)
      )
    ))

(def runs (map str raw-alphabet (rest raw-alphabet) (rest (rest raw-alphabet))))
(defn rule-one [candidate]
  "returns nil if there are no runs of three alphabetical letters."
  (some #(.contains candidate %) runs))

(defn rule-three [candidate]
  "returns nil if there are fewer than two different consecutive pairs."
  (not-empty (drop 1 (distinct (map first (re-seq #"(.)\1" candidate))))))

(def santas-next-passwords
  (filter #(and (rule-one %) (rule-three %))
          (rest (iterate
                 (partial next-string alph)
                 "hxbxwxba"))
          ))

; part 1
(first santas-next-passwords)

; part 2
(second santas-next-passwords)