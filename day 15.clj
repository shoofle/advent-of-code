(def test-ingredients {"Butterscotch" {:capacity -1 :durability -2 :flavor 6 :texture 3 :calories 8}
                       "Cinnamon" {:capacity 2 :durability 3 :flavor -2 :texture -1 :calories 3}
                       })

(def recipe {"Butterscotch" 44 "Cinnamon" 56})

(def empty-properties {:capacity 0
                       :durability 0
                       :flavor 0
                       :texture 0})

(defn property-scores [ingredients recipe]
  (reduce (fn [properties [ingredient quantity]] 
            (merge-with (fn [running-total ing-score]
                          (+ running-total (* ing-score quantity)))
                        properties
                        (get ingredients ingredient)))
          (zipmap (keys (first (vals test-ingredients))) (repeat 0)) ;properties of an ingredient
          recipe))

(defn score [ingredients recipe] (reduce (fn [total individual]
                                           (* total (max individual 0)))
                                         (-> (property-scores ingredients recipe)
                                             (select-keys (keys empty-properties))
                                             vals)
                                         ))

(score test-ingredients recipe)

(def full-ingredients {"Sprinkles" {:capacity 5, :durability -1, :flavor 0, :texture 0, :calories 5}
                       "PeanutButter" {:capacity -1, :durability 3, :flavor 0, :texture 0, :calories 1}
                       "Frosting" {:capacity 0, :durability -1, :flavor 4, :texture 0, :calories 6}
                       "Sugar" {:capacity -1, :durability 0, :flavor 0, :texture 2, :calories 8}
                       })

(def all-improvisations 
  (fn [space ingredients]
    (if (empty? (rest ingredients))
      [{(first ingredients) space}]
      (mapcat 
       (fn [amount] 
         (map (fn [recipe] (assoc recipe (first ingredients) amount))
              (all-improvisations (- space amount) (rest ingredients))))
       (range (inc space)))
      )
    )
  )

(reduce max (map (partial score test-ingredients) (all-improvisations 100 ["Butterscotch" "Cinnamon"])))
(score test-ingredients recipe)

(defn find-best-recipe [space ingredients-list]
  (->> (keys ingredients-list)
       (all-improvisations space)
       (map (partial score ingredients-list))
       (reduce max))
  )

;part 1! equiv to:
;(reduce max (map (partial score full-ingredients) (all-improvisations 100 (keys full-ingredients))))
(find-best-recipe 100 full-ingredients)

;part 2!
;we need to get only recipes with exactly 500 calories per cookie.
(->> (keys full-ingredients) ;start with all the ingredients we have available
     (all-improvisations 100) ;iterate over all the recipes using those ingredients
     (filter #(= (get (property-scores full-ingredients %) :calories) 500)) ;select only the ones with 500 calories
     (map #(score full-ingredients %)) ;calculate scores
     (reduce max)) ;find best-scoring recipe
