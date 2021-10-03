;&optional must be before &key
;must be all &optional before any &key arguments
(defun fun1 (a b &optional (c 1)) (+ a b c))
(fun1 10 100) => "111"

(defun fun2 (a b &optional (c 1) &key (d 2)) (+ a b c d))
fun2 10 100 1 :d 3) => "114"
;(fun2 10 100 :d 3) =e> "FUN2: keyword arguments in (3) should occur pairwise"


;(defun fun3 (a b &key (c 1) &optional (d 2)) (+ a b c d)) =e> " DEFUN: Lambda list marker &OPTIONAL not allowed here."

(defun fun4 (a b &optional c (d 1) &key (e 2)) (+ a b c d e))
(fun4 1 2 3 4 :e 5) => "15"
(fun4 1 2 3 :e 5) =e> "FUN4: keyword arguments in (5) should occur pairwise"
(fun4 1 2 3 4) => "12"
(fun4 1 2 3) => "9"



