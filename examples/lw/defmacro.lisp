;http://www.lispworks.com/documentation/lw50/CLHS/Body/m_defmac.htm


 (defmacro mac1 (a b) "Mac1 multiplies and adds"
            `(+ ,a (* ,b 3))) =>  MAC1
 (mac1 4 5) =>  19
 (documentation 'mac1 'function) =>  "Mac1 multiplies and adds"
 (defmacro mac2 (&optional (a 2 b) (c 3 d) &rest x) `'(,a ,b ,c ,d ,x)) =>  MAC2
 (mac2 6) =>  (6 T 3 NIL NIL)
 (mac2 6 3 8) =>  (6 T 3 T (8))
 (defmacro mac3 (&whole r a &optional (b 3) &rest x &key c (d a))
    `'(,r ,a ,b ,c ,d ,x)) =>  MAC3
 (mac3 1 6 :d 8 :c 9 :d 10) =>  ((MAC3 1 6 :D 8 :C 9 :D 10) 1 6 9 8 (:D 8 :C 9 :D 10))

