(defun factorial (n)
  (cond
    ((= n 1) 1)
    (1 (* n (factorial (- n 1))))))
(print (factorial 3))
