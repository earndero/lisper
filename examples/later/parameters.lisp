; functions with params, &key and &rest are incompatible
; &optional - default parameters must be last
(defun fwp (a b c &optional d e f &key g h i &rest j &aux k l m) (+ a b c d e f g h i j k l m)) =e> "DEFUN: Lambda list marker &REST not allowed here."

(defun fwp1 (a b c &optional (d 1) (e 2) (f 3) (g 4) (h 5) (i 6) &rest j &aux (k 10) (l 20) (m 30)) (+ a b c d e f g h i k l m))

(defun fwp2 (a b c &optional d e (f 1000) &key g (h 12) (i 13) j &aux (k 10) (l 20) (m 30)) (+ a b c d e f g h i j k l m))

(defun fwp3 (a b c &optional d e (f 1000) &aux (k 10) (l 20) (m 30)) (+ a b c d e f k l m))

(fwp1 1 2 3) => "87"

(fwp2 1 2 3 10 20 30 :g 1 :j 100) => "252"

(fwp3 1 2 3 10 20) => "1096"

