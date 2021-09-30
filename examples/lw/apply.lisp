;http://www.lispworks.com/documentation/lw50/CLHS/Body/f_apply.htm#apply
 (setq f '+) =>  +
 (apply f '(1 2)) =>  3
 (setq f #'-) =>  #<FUNCTION ->
 (apply f '(1 2)) =>  -1
 (apply #'max 3 5 '(2 7 3)) =>  7
 (apply 'cons '((+ 2 3) 4)) =>  ((+ 2 3) . 4)
 (apply #'+ '()) =>  0

 (defparameter *some-list* '(a b c))
 (defun strange-test (&rest x) (eq x *some-list*))
 (apply #'strange-test *some-list*) =>  implementation-dependent

 (defun bad-boy (&rest x) (rplacd x 'y))
 (bad-boy 'a 'b 'c) has undefined consequences.
 (apply #'bad-boy *some-list*) has undefined consequences.

 (defun foo (size &rest keys &key double &allow-other-keys)
   (let ((v (apply #'make-array size :allow-other-keys t keys)))
     (if double (concatenate (type-of v) v v) v)))
 (foo 4 :initial-contents '(a b c d) :double t)
    =>  #(A B C D A B C D)

