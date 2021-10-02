(defun defval
  (&key
    (a 5)
    (b 10)
  )
  (+ a b)) => "DEFVAL"

(defval) => "15"
(defval :b 100) => "105"
(defval :b 100 :a 10) => "110"

;(defval :b 100 a:10) =e> "there is no package with name \"A\""

