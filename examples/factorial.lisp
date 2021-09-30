(defun factorial (n)
  (cond
    ((= n 1) 1)
    (t (* n (factorial (- n 1))))))
(factorial 40) => 815915283247897734345611269596115894272000000000
