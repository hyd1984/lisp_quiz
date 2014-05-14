;p01
;Find the last box of a list.
;    Example:
;        * (my-last '(a b c d))
;            (D)
(defun my-last (lst)
  (if (eq (cadr lst) nil)
	lst
	(my-last (cdr lst))))

;p02
;Find the last but one box of a list.
;    Example:
;        * (my-but-last '(a b c d))
;            (C D)
(defun my-but-last (lst)
  (if (eq (caddr lst) nil)
	lst
	(my-but-last (cdr lst))))

;P03
; Find the K'th element of a list.
;     The first element in the list is number 1.
;         Example:
;             * (element-at '(a b c d e) 3)
;                 C
(defun element-at (lst n)
  (if (= n 1)
	(car lst)
	(element-at (cdr lst) (1- n))))

;P04
;Find the number of elements of a list.
(defun n-of-list (lst)
  (if (eq lst nil)
	0
	(1+ (n-of-list (cdr lst)))))

;P05
;Reverse a list.
(defun my-reverse (lst)
  (my-reverse-aux lst nil))

(defun my-reverse-aux (lst aux)
  (if (eq lst nil)
	aux
	(my-reverse-aux (cdr lst) (cons (car lst) aux))))

;P06
;Find out whether a list is a palindrome.
;    A palindrome can be read forward or backward; e.g. (x a m a x)
(defun eq-list (lst1 lst2)
  (labels ((neqcar (lst1 lst2) (not (eq (car lst1) (car lst2)))))
		 (cond ((and (eq (car lst1) (car lst2)) (eq (car lst1) nil)) t)
			   ((neqcar lst1 lst2) nil)
			   (t (eq-list (cdr lst1) (cdr lst2))))))
(defun palindrome (lst)
  (if (eq-list lst (reverse lst))
	t
	nil))

;P07
;Flatten a nested list structure.
;    Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
;
;        Example:
;            * (my-flatten '(a (b (c d) e)))
;                (A B C D E)
(defun pair (x) (not (atom x)))
(defun flatten (lst)
  (cond  ((null lst) nil)
	((atom (car lst)) (cons (car lst) (flatten (cdr lst))))
		((pair (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
		))
