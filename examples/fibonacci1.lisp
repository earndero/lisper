(defun fibonacci (n &optional (a 0) (b 1) (acc ()))
  (if (zerop n)
      (nreverse acc)
      (fibonacci (1- n) b (+ a b) (cons a acc))))

(fibonacci 5) => (0 1 1 2 3)
