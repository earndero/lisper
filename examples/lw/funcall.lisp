; http://www.lispworks.com/documentation/lw50/CLHS/Body/f_funcal.htm
 (funcall #'+ 1 2 3) =>  6
 (funcall 'car '(1 2 3)) =>  1
 (funcall 'position 1 '(1 2 3 2 1) :start 1) =>  4
 (cons 1 2) =>  (1 . 2)
 (flet ((cons (x y) `(kons ,x ,y)))
   (let ((cons (symbol-function '+)))
     (funcall #'cons
              (funcall 'cons 1 2)
              (funcall cons 1 2))))
=>  (KONS (1 . 2) 3)
