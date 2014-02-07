;; I used to start from the end. Let's define the core predicate for the task.

(defn row-columns [x]
  (concat x (apply map list x)))

(defn latin? [x]  
  (every? 
   #(and 
     (= (set %) (set (flatten x))) 
     (apply = (vals (frequencies %)))) 
   (row-columns x))    
)

;; utilitary function
;; [a [b c] ... ] -> [[a b ... ] [a c ...]]

(defn unzip-seq [c]
  (reduce 
   (fn [c e] 
     (reduce 
      #(concat % (map (fn [p] (conj p %2)) c)) 
      [] e)) 
   [[]] c))

;; generate all possible alignments for x
;; [[:a :b :c] [:f]] ->  [[0 0] [0 1] [0 2]]

(defn alignments [x]
  (let [width (last (sort (map count x)))]
    (vec (unzip-seq (map #(range (inc (- width (count %)))) x)))))

;; sort of get-in which works for aligned seq

(defn profile [s] (mapv #(set (keys %)) s))

(defn s-map [a x]
  (mapv #(zipmap (range % (+ % (count %2))) %2) a x))

;; let's seek in the deep on every (but last) levels
;; for each row we have seq of sets Ai with property 
;; Ai+1 belongs to Ai for every i
(defn mining [s]   
  (map
   (fn [r] (reductions (fn [a b] (set (filter a b))) r))
   (drop-last (take-while seq (iterate next (profile s))))))

;; utilitary function, takes an arbitrary matrix 'm' and produces 
;; square matrices of length '(count m)' by horizontal sliding 
(defn h-slice [m]
  (let [n (count m)]
    (if (< (count (last m)) n) []
        (concat [(map #(take n %) m)] (h-slice (map next m))))))

;; use mining data to generate rectangular maps
(defn gen-maps-r [s]  
  (for [ m (mapv vector (iterate next s) (mining s))
        [i c] (map list (range 1 (count (m 1))) (next (m 1))) 
        :while (> (count c) i)]
    (map #(replace % %2) (m 0) (take (inc i) (repeat c)))))

;; what a nice word 'lift'

(defn lift-data [m s]
  (map #(replace (s %) (m %)) (range (count m)))) 

(fn [x]
  (letfn [
          (row-columns [x] (concat x (apply map list x)))
          (latin? [x] 
            (every? 
             #(and 
               (= (set %) (set (flatten x))) 
               (apply = (vals (frequencies %)))) 
             (row-columns x)))
          (unzip-seq [c]
            (reduce 
             (fn [c e] 
               (reduce 
                #(concat % (map (fn [p] (conj p %2)) c)) 
                [] e)) 
             [[]] c))
          (alignments [x]
            (let [width (last (sort (map count x)))]
              (vec (unzip-seq (map #(range (inc (- width (count %)))) x)))))
          (profile [s] (mapv #(set (keys %)) s))
          (s-map [a x]
            (mapv #(zipmap (range % (+ % (count %2))) %2) a x))
          (mining [s]   
            (map
             (fn [r] (reductions (fn [a b] (set (filter a b))) r))
             (drop-last (take-while seq (iterate next (profile s))))))
          (h-slice [m]
            (let [n (count m)]
              (if (< (count (last m)) n) []
                  (concat [(map #(take n %) m)] (h-slice (map next m))))))
          (gen-maps [s]  
            (for [ m (mapv vector (iterate next s) (mining s))
                  [i c] (map list (range 1 (count (m 1))) (next (m 1))) 
                  :while (> (count c) i)]
              (map #(replace % %2) (m 0) (take (inc i) (repeat c)))))
          (lift-data [m s]
            (map #(replace (s %) (m %)) (range (count m))))           
          ]
    (->> x
         (alignments)
         (map #(s-map % x))
         (mapcat #(->> %
                       (gen-maps)
                       (mapcat h-slice)
                       (filter latin?)))
         (distinct)
         (map count)
         (frequencies))))
