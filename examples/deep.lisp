(defun deep (n)
  (cond
    ((= n 1) 1)
    (t (+ 1 (deep (- n 1))))))

(deep 3000) => 3000
