; http://www.lispworks.com/documentation/lw60/CLHS/Body/02_dh.htm
(defun fvec
  (&key
    (a #(5 7))   ; no space between # and next char
    (b #(10 1 2)) ; without # must be error in defun
  )
  (length b))

(fvec) => 3

