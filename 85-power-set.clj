(defn f [s]
    ((fn g [q w]
       (if (= q #{}) q 
           (reduce #(clojure.set/union % 
                                       (g (disj q %2) 
                                          (conj w %2))) 
                   (conj #{} w q) q))) 
     s #{})) 

(f #{} [#{1 2 3 4 5}])

(f #{} [(into #{} (range 10))])

#(% #{%2} %2)
(fn [r s]
    (if (seq s) 
        (let [o (map #(disj (first s) %) (first s))]          
          (recur (reduce conj r o) (reduce conj (next s) o))
          ) 
        r))

(defn add-x [s x]
  (concat s (map #(conj % x) s)))

(defn main [s] (reduce (fn [r x] (reduce conj r (map #(conj % x) r))) #{#{}} s))
(fn [s] (reduce (fn [r x] (reduce conj r (map #(conj % x) r))) #{#{}} s))

(add-x [[1 2 3 4] [2 3 4] [2 3] [1] []] :a)
(add-x [] :a)

(main #{1 :a})
