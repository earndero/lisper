(defun f0 (a b) (+a b)) => F0
(f0 4 6) =e> undefined function +A

(defun f1 (&key a b) (+ a b))
(f1 4 6) =e> F1: &KEY marker 4 is not a symbol

(sum 1 2 3) =e> undefined function SUM
