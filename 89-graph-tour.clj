(defn has-eulerian-path [y]
  (letfn [
          (near-edges [[_ y :as e] g]
            (if (nil? e) (into (set g) (map reverse g))
                (map (fn [[a b]] (if (= y b) [b a] [a b])) (filter #((set %) (last e)) g))))
          (remove-edge [e g]
            (let [[f n] (split-with #(and (not= e %) (not= (reverse e) %)) (vec g))] 
              (apply concat [f (next n)])))
          
          (next-edge [v g]
            (let [n (near-edges v g)]    
              (do (prn v g n)
                  (if (empty? n) 
                    (empty? g)          
                    (some #(next-edge % (remove-edge % g)) n)))))          
          ]
    (true? (next-edge nil y))))

(= true (has-eulerian-path [[1 0] [1 2]]))
(= true (has-eulerian-path [[:a :b]]))
(= true (has-eulerian-path [[1 2] [2 3] [3 4] [4 1]]))
(= true (has-eulerian-path [[:a :b] [:a :c] [:c :b] [:a :e] [:b :e] [:a :d] [:b :d] [:c :e] [:d :e] [:c :f] [:d :f]]))

(= false (has-eulerian-path [[:a :b] [:a :b] [:a :c] [:c :a] [:a :d] [:b :d] [:c :d]]))
(= false (has-eulerian-path [[:a :a] [:b :b]]))
(= false (has-eulerian-path [[1 2] [2 3] [2 4] [2 5]]))

