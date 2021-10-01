; http://www.lispworks.com/documentation/lw60/CLHS/Body/m_do_do.htm

 (do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1- temp-two)))
      ((> (- temp-one temp-two) 5) temp-one)) =>  4

 (do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1+ temp-one)))
      ((= 3 temp-two) temp-one)) =>  3

 (do* ((temp-one 1 (1+ temp-one))
        (temp-two 0 (1+ temp-one)))
       ((= 3 temp-two) temp-one)) =>  2

 (do ((j 0 (+ j 1)))
     (nil)                       ;Do forever.
   (format t "~%Input ~D:" j)
   (let ((item (read)))
     (if (null item) (return)   ;Process items until NIL seen.
         (format t "~&Output ~D: ~S" j item))))
>>  Input 0: banana
>>  Output 0: BANANA
>>  Input 1: (57 boxes)
>>  Output 1: (57 BOXES)
>>  Input 2: NIL
=>  NIL

 (setq a-vector (vector 1 nil 3 nil))
 (do ((i 0 (+ i 1))     ;Sets every null element of a-vector to zero.
      (n (array-dimension a-vector 0)))
     ((= i n))
   (when (null (aref a-vector i))
     (setf (aref a-vector i) 0))) =>  NIL
a-vector =>  #(1 0 3 0)

 (do ((x e (cdr x))
      (oldx x x))
     ((null x))
   body)



