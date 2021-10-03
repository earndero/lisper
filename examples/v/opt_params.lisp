(defun defval
  (a
  &optional
   b
   (c 10)
  )
  (+ a b c)) => "DEFVAL"

(defval 3 4) => "17"
(defval 3 4 100) => "107"


