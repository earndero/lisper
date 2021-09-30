; http://www.lispworks.com/documentation/lw60/CLHS/Body/s_let_l.htm
 (setq a 'top) =>  TOP
 (defun dummy-function () a) =>  DUMMY-FUNCTION
 (let ((a 'inside) (b a))
    (format nil "~S ~S ~S" a b (dummy-function))) =>  "INSIDE TOP TOP"
 (let* ((a 'inside) (b a))
    (format nil "~S ~S ~S" a b (dummy-function))) =>  "INSIDE INSIDE TOP"
 (let ((a 'inside) (b a))
    (declare (special a))
    (format nil "~S ~S ~S" a b (dummy-function))) =>  "INSIDE TOP INSIDE"
