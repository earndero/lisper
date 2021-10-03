(defun defval
  (&key
    (a 5)
    (b 10)
   &aux
    (c 1)
  )
  (+ a b c)) => "DEFVAL"

(defval) => "15"
(defval :b 100) => "105"
(defval :b 100 :a 10) => "110"

;https://riptutorial.com/common-lisp/example/6984/auxiliary-variables
(defun foobar (x y &aux (z (+ x y)))
  (format t "X (~d) and Y (~d) are required.~
             Their sum is ~d."
          x y z))

(foobar 10 20)
; X (10) and Y (20) are required.
; Their sum is 30.
;=> NIL

