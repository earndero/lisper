http://www.lispworks.com/documentation/lw50/CLHS/Body/f_map.htm

 (map 'string #'(lambda (x y)
                  (char "01234567890ABCDEF" (mod (+ x y) 16)))
       '(1 2 3 4)
       '(10 9 8 7)) =>  "AAAA"
 (setq seq '("lower" "UPPER" "" "123")) =>  ("lower" "UPPER" "" "123")
 seq =>  ("LOWER" "UPPER" "" "123")
 (map 'list #'- '(1 2 3 4)) =>  (-1 -2 -3 -4)
 (map 'string
      #'(lambda (x) (if (oddp x) #\1 #\0))
      '(1 2 3 4)) =>  "1010"

 (map '(vector * 4) #'cons "abc" "de") should signal an error

