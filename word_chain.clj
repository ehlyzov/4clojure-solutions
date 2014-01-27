#(true? (% %2 %2))
(fn f [r v] 
  (if (empty? r) true 
      (some (fn [z] (f (disj r z) 
            (set (filter 
                  #(
                    (fn h [[q & x :as o] [w & y :as p]]  
                      (if (some empty? [o p])
                        (>= 1 (+ (count o) (count p)))
                        (if (= q w) (h x y) 
                          (or (= x y) (= x (seq p)) (= (seq o) y))))
                      ) z %) r)))) v)))

#{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}


(filter #(comp (fn [x] (> x 0)) ()))
