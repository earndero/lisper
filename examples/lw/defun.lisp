;http://www.lispworks.com/documentation/lw50/CLHS/Body/m_defun.htm
 (defun recur (x)
  (when (> x 0)
    (recur (1- x)))) =>  RECUR
 (defun ex (a b &optional c (d 66) &rest keys &key test (start 0))
    (list a b c d keys test start)) =>  EX
 (ex 1 2) =>  (1 2 NIL 66 NIL NIL 0)
 (ex 1 2 3 4 :test 'equal :start 50)
=>  (1 2 3 4 (:TEST EQUAL :START 50) EQUAL 50)
 (ex :test 1 :start 2) =>  (:TEST 1 :START 2 NIL NIL 0)

 ;; This function assumes its callers have checked the types of the
 ;; arguments, and authorizes the compiler to build in that assumption.
 (defun discriminant (a b c)
   (declare (number a b c))
   "Compute the discriminant for a quadratic equation."
   (- (* b b) (* 4 a c))) =>  DISCRIMINANT
 (discriminant 1 2/3 -2) =>  76/9

 ;; This function assumes its callers have not checked the types of the
 ;; arguments, and performs explicit type checks before making any assumptions.
 (defun careful-discriminant (a b c)
   "Compute the discriminant for a quadratic equation."
   (check-type a number)
   (check-type b number)
   (check-type c number)
   (locally (declare (number a b c))
     (- (* b b) (* 4 a c)))) =>  CAREFUL-DISCRIMINANT
 (careful-discriminant 1 2/3 -2) =>  76/9

