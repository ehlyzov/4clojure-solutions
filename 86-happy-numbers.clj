(defn digits [n]
  (if (= 0 n) [] (concat (digits (quot n 10)) [(mod n 10)])))

(defn main [r n]
  (and 
   (not (r n)) 
   (or 
    (= 1 n) 
    (main (conj r n) (reduce #(+ % (* %2 %2)) 0 
                             ((fn d [n] (if (= 0 n) 
                                           [] 
                                           (concat 
                                            (d (quot n 10)) 
                                            [(mod n 10)]))) n))))))

(main #{} 7)
(main #{} 986543210)
(main #{} 2)
(main #{} 3)
