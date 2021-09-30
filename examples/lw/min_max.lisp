; http://www.lispworks.com/documentation/lw50/CLHS/Body/f_max_m.htm
; clisp returns int not converted to double
 (max 3) =>  3
 (min 3) =>  3
 (max 6 12) =>  12
 (min 6 12) =>  6
 (max -6 -12) =>  -6
 (min -6 -12) =>  -12
 (max 1 3 2 -7) =>  3
 (min 1 3 2 -7) =>  -7
 (max -2 3 0 7) =>  7
 (min -2 3 0 7) =>  -2
 (max 5.0 2) =>  5.0
 (min 5.0 2) =>  2
 (max 3.0 7 1) => 7
 (min 3.0 7 1) => 1
 (max 1.0s0 7.0d0) =>  7.0d0
 (min 1.0s0 7.0d0) =>  1.0s0
 (max 3 1 1.0s0 1.0d0) =>  3
 (min 3 1 1.0s0 1.0d0) =>  1

