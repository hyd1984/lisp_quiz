;q21
;Insert an element at a given position into a list.
;    Example:
;        * (insert-at 'alfa '(a b c d) 2)
;            (A ALFA B C D)
(defun insert-at (elem lst n)
  (cond ((null lst) lst)
		((= n 1) (cons (car lst) (cons elem (insert-at elem (cdr lst) (1- n)))))
		(t (cons (car lst) (insert-at elem (cdr lst) (1- n))))))
;q22
;Create a list containing all integers within a given range.
;    If first argument is smaller than second, produce a list in decreasing order.
;        Example:
;            * (range 4 9)
;                (4 5 6 7 8 9)
(defun range (start end)
  (if (= start end) 
	(cons  end nil)
	(cons start (range (1+ start) end))))
;q26
;Generate the combinations of K distinct objects chosen from the N elements of a list
;    In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
;
;        Example:
;            * (combination 3 '(a b c d e f))
;                ((A B C) (A B D) (A B E) ... ) 
(defun cn1 (lst)
  (if (null lst)
	nil
	(cons (list (car lst)) (cn1 (cdr lst)))))
(defun after-elem (elem lst)
  (if (eq elem (car lst))
	(cdr lst)
	(after-elem elem (cdr lst))))
(defun attach-elem (lst1 lst2)
  (let ((remain-elem (after-elem (car (reverse lst1)) lst2)))
	(if (null lst1)
	  (cn1 lst2)
	  (attach-elem-aux lst1 remain-elem nil))))

(defun attach-elem-aux (lst remain res)
  (labels ((add-tail (elem lst) (reverse (cons elem (reverse lst)))))
	(if (null remain)
	  (reverse res)
	  (attach-elem-aux lst (cdr remain) (cons (add-tail (car remain) lst) res)))))

(defun attach-list (lst1 lst2)
  (if (null lst1)
	nil
	(append (attach-elem (car lst1) lst2) (attach-list (cdr lst1) lst2))))
(defun combination (n lst)
  (if (= n 1)
	(cn1 lst)
	(attach-list (combination (1- n) lst) lst)))
