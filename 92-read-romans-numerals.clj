;; Roman numerals are easy to recognize, but not everyone knows all the rules 
;; necessary to work with them. Write a function to parse a Roman-numeral string
;; and return the number it represents. 

;; You can assume that the input will be well-formed, in upper-case, and follow 
;; the subtractive principle. You don't need to handle any numbers greater than 
;; MMMCMXCIX (3999), the largest number representable with ordinary letters.

#(apply + (map 
           {
            "I" 1
            "X" 10
            "C" 100
            "V" 5
            "L" 50
            "D" 500
            "M" 1000   
            "IV" 4
            "IX" 9
            "XL" 40
            "XC" 90
            "CD" 400
            "CM" 900
            }
           (re-seq  #"IV|IX|XL|XC|CD|CM|I|X|C|V|L|D|M" %)))

