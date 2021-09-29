; from http://bacon.umcs.lublin.pl/~pgiza/dyd/lispouczek/lk.php-nr=11.htm
(setq a 3)  ;  3

(print (cond
  ((evenp a) a) 		;if a is even return a
  ((> a 7) (/ a 2)) 	;otherwise if a > 7 return a/2
  ((< a 5) (- a 1))	 	;otherwise if a < 5 return a-1
  (t 17)    ;otherwise return 17
))       ;=> 2
