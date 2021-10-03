; sign ' inside string
(print "123;456")

(defun shfunc
  (&key
    (xini #(4.0 -5.0))
    (vmin #(0.01 0.01))
  )
  (+ xini vmin))

(shfunc :xini 1 :vmin 2)
(shfunc)
