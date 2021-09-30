(defun defval
  (&key
    (a 5)
    (b 10)
  )
  (+ a b))

(defval) => 15
(defval :b 100) => 105

