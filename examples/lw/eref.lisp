http://www.lispworks.com/documentation/lw50/CLHS/Body/f_aref.htm
 (aref (setq alpha (make-array 4)) 3) =>  NIL
 (setf (aref alpha 3) 'sirens) =>  SIRENS
 (aref alpha 3) =>  SIRENS
 (aref (setq beta (make-array '(2 4)
                    :element-type '(unsigned-byte 2)
                    :initial-contents '((0 1 2 3) (3 2 1 0))))
        1 2) =>  1
 (setq gamma '(0 2))
 (apply #'aref beta gamma) =>  2
 (setf (apply #'aref beta gamma) 3) =>  3
 (apply #'aref beta gamma) =>  3
 (aref beta 0 2) =>  3
