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
; Calculate Euler's totient function phi(m).
;     Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
;
;         Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
;
;             * (totient-phi 10)
;                 4
;
;                     Find out what the value of phi(m) is if m is a prime number. Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA). In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later).
(defun under-n (n)
  (if (= n 1)
	'(1)
	(cons n (under-n (1- n)))))
(defun coprime-list (num under)
  (cond ((= num (car under)) (coprime-list num  (cdr under)))
		((= 1 (car under)) '(1))
		((eq t (coprime num (car under))) (cons (car under) (coprime-list num (cdr under))))
		(t (coprime-list num (cdr under)))))
(defun totient-phi (num)
  (length (coprime-list num (under-n num))))
;q35
;
(defun n-to-m (start end)
  (if (= start end)
	(list start)
	(cons end (n-to-m start (1- end)))))
(defun prime-list (start end)
	(reverse (remove-if-not #'is-prime (n-to-m start end))))
(defun floor-sqrt (num)
  (car (multiple-value-bind (f r) (floor (sqrt num)) (list f r))))
(defun prime-factors (num)
  (let ((prime-lst (prime-list 2 (floor-sqrt num))))
	(prime-factor-aux num prime-lst)))
(defun prime-factor-aux (num prime-lst)
  (cond ((null prime-lst) nil)
		((= num 1) nil)
	    ((= (mod num (car prime-lst)) 0) (cons (car prime-lst) (prime-factor-aux (/ num (car prime-lst)) prime-lst)))
		(t (prime-factor-aux num (cdr prime-lst)))))
;q39
;(prime-list start end)
;q40
(load "quiz21-26.lsp")
(defun goldbach (n)
  (remove-if-not #'(lambda (elem) (= n (+ (car elem) (cadr elem)))) (combination 2 (prime-list 2 n))))
