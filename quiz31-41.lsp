;q31
; Determine whether a given integer number is prime.
;     Example:
;         * (is-prime 7)
;             T
(defun is-prime-aux (num given)
  (cond ((= num (* given given)) nil)
		((< num (* given given)) t)
		((= (mod num given) 0) nil)
		(t (is-prime-aux num (1+ given)))))
(defun is-prime (num)
  (is-prime-aux num 2))
;q32
;Determine the greatest common divisor of two positive integer numbers.
;    Use Euclid's algorithm.
;        Example:
;            * (gcd 36 63)
;                9
(defun my-gcd (num1 num2)
  (if (= (mod num2 num1) 0)
	num1
	(my-gcd (mod num2 num1) num1)))
;q33
;Determine whether two positive integer numbers are coprime.
;    Two numbers are coprime if their greatest common divisor equals 1.
;        Example:
;            * (coprime 35 64)
;                T
(defun coprime (num1 num2)
  (if (= (gcd num1 num2) 1) 
	t
	nil))
;q34

