;&optional must be before &key
(defun fun1 (a b &optional (c 1)) (+ a b c))
(fun1 10 100) => "111"

(defun fun2 (a b &optional (c 1) &key (d 2)) (+ a b c d))
fun2 10 100 1 :d 3) => "114"
(fun2 10 100 :d 3) =e> "FUN2: keyword arguments in (3) should occur pairwise"


(defun fun3 (a b &key (c 1) &optional (d 2)) (+ a b c d)) =e> " DEFUN: Lambda list marker &OPTIONAL not allowed here."
