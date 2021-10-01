;http://www.lispworks.com/documentation/lw50/CLHS/Body/s_if.htm
 (if t 1) =>  1
 (if nil 1 2) =>  2
 (defun test ()
   (dolist (truth-value '(t nil 1 (a b c)))
     (if truth-value (print 'true) (print 'false))
     (prin1 truth-value))) =>  TEST
 (test)
>>  TRUE T
>>  FALSE NIL
>>  TRUE 1
>>  TRUE (A B C)
=>  NIL
