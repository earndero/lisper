http://www.lispworks.com/documentation/HyperSpec/Body/03_dad.htm
(defun f0 (a b) (+ a b))
(f0 4 6) => 10

(defun f1 (&key a b) (+ a b))
(f1 :a 4 :b 6 ) => 10

(defun f2 (c &key a b) (+ a b c))
(f2 1 :a 4 :b 6) => 11
