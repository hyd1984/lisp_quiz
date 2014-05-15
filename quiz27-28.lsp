;p27b
;
(defun before-n (lst n)
  (if (= n 0)
	nil
	(cons (car lst) (before-n (cdr lst) (1- n)))))
(defun after-n (lst n)
  (if (= n 0)
	lst
	(after-n (cdr lst) (1- n))))
(defun group (lst n-lst)
  (if (null n-lst) 
	nil
	(cons (before-n lst (car n-lst)) (group (cdr lst) (cdr n-lst)))))
;p28a
(defun lsort (lst)
  (sort lst #'< :key #'length ))
