(defn add-edge [g [a b]]
  (let [r (reduce conj (g a #{a}) (g b #{b}))]
    (reduce #(assoc % %2 r) g r)))

(defn main [x]
  (apply = (vals (reduce add-edge {} x))))

(and
(= true (main #{[:a :a]}))
(= true (main #{[:a :b]}))
(= false (main #{[1 2] [2 3] [3 1]
               [4 5] [5 6] [6 4]}))
(= true (main #{[1 2] [2 3] [3 1]
              [4 5] [5 6] [6 4] [3 4]}))
(= false (main #{[:a :b] [:b :c] [:c :d]
               [:x :y] [:d :a] [:b :e]}))
(= true (main #{[:a :b] [:b :c] [:c :d]
              [:x :y] [:d :a] [:b :e] [:x :a]}))
 )


;;;;;;;;


(fn [s]
  (apply = (vals (reduce 
                  (fn [g [a b]]
                    (let [r (reduce conj (g a #{a}) (g b #{b}))]
                      (reduce #(assoc % %2 r) g r)))
                  {} s))))

