;(setq vec #(3.4))=e>comma is illegal outside of backquote
(setq vec #(3 4)) => "#(3 4)"
(aref vec 0) => "3"
(setf (aref vec 0) 5) => "5"
;(setq (aref vec 0) 5) =e>

(defun sumvec (vec) (+ (aref vec 0) (aref vec 1))) => "SUMVEC"
(sumvec #(3 4)) => 7

(defun sumvec2 (&key (vec #(31 42))) (+ (aref vec 0) (aref vec 1))) => "SUMVEC2"
(sumvec2) => "73"
