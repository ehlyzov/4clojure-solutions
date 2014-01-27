(require '[clojure.set :refer [union difference]])

(defn l [[x y] [i j]] 
  (disj (set [(if (= y i) [x j]) (if (= j x) [i y])]) nil))

(defn filter-set2 [x y] 
  (let [
        k (reduce #(union % (apply union (map (partial l %2) %))) y x)
        ]
    (if (= k y) k (filter-set2 x k))))

(defn filter-set2 [x y] 
  (let [k (reduce #(reduce conj % %2) y (for [i x j y] (l i j)))]    
    (if (= k y) k (filter-set2 x k))))

(defn f [o x]
  (if (empty? x) o 
      (#(f (reduce conj o %) (reduce disj x %)) 
       ((fn r [b n]
          (let [k (reduce #(reduce conj % %2) n 
                           (for [i b j n] (
                                           (fn [[x y] [i j]] 
                                             (disj (set [(if (= y i) [x j]) (if (= j x) [i y])]) nil))
                                           i j)))]
            (if (= k n) k (r b k)))) x #{(first x)}))))

(comment "Good solution") 

#(loop [s %]
   (let [n (reduce conj s
                   (for [[a b] s [c d] s 
                         :when (= b c)] 
                     [a d]))]
      (if (= n s) n (recur n))))
              
(f #{} #{[8 4] [9 3] [4 2] [27 9]})

((juxt #(reduce conj #{} %) #(reduce disj #{} %)) #{1 2 3})
