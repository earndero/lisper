;http://www.lispworks.com/documentation/lw50/CLHS/Body/s_fn.htm#function
 (defun adder (x) (function (lambda (y) (+ x y))))

;The result of (adder 3) is a function that adds 3 to its argument:
 (setq add3 (adder 3))
 (funcall add3 5) =>  8

