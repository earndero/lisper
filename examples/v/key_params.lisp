(defun f2 (c &key a b) (+ a b c)) => "F2"
(f2 1 :a 4 :b 6) => "11"

(defun fun1 (a b &key (c 1)) (+ a b c)) => "FUN1"
(fun1 10 100) => "111"

