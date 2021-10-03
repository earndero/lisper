; rest like varargs in C or ...
(defun funrest (a b &rest c d)(+ a b)) =e> "DEFUN: Lambda list element D is superfluous. Only one variable is allowed after &REST"

(defun funrest2 (a b &rest c)(+ a b c)) => "FUNREST2"
(funrest2 1 2 3) =e> "+: (3) is not a number"
(funrest2 1 2) =e> "+: NIL is not a number"

(defun funrest3 (a b &rest c)(+ a b (car c))) => "FUNREST3"
(funrest3 1 2 3) => "6"
