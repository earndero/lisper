; http://www.lispworks.com/documentation/lw51/CLHS/Body/02_dh.htm
; '              function abbreviation
; http://www.lispworks.com/documentation/lw50/CLHS/Body/f_map.htm
 (map 'string #'(lambda (x y)
                  (char "01234567890ABCDEF" (mod (+ x y) 16)))
       '(1 2 3 4)
       '(10 9 8 7)) =>  "AAAA"

 (map nil #'nstring-upcase seq) =>  NIL

 (map 'list #'- '(1 2 3 4)) =>  (-1 -2 -3 -4)
 (map 'string
      #'(lambda (x) (if (oddp x) #\1 #\0))
      '(1 2 3 4)) =>  "1010"

