;q14
(defun dupli (lst)
  (if (null lst) 
	nil
	(cons (car lst) (cons (car lst) (dupli (cdr lst))))))
;q15
(defun n-rep (elem n)
  (if (zerop n) nil
	(cons elem (n-rep elem (1- n)))))

(defun repli (lst n)
  (if (null lst) nil
	(append (n-rep (car lst) n) (repli (cdr lst) n))))
;q16
(defun drop-aux (lst n cnt)
  (labels ((be-dropped (cnt n) (if (= (mod (1+ cnt) n) 0) t nil)))
	(cond ((null lst) nil)
		  ((be-dropped cnt n) (drop-aux (cdr lst) n (1+ cnt)))
		  (t (cons (car lst) (drop-aux (cdr lst) n (1+ cnt)))))))

(defun drop (lst n)
  (drop-aux lst n 0))

;q17
(defun left-n (lst n)
  (if (> n 0)
	(cons (car lst) (left-n (cdr lst) (1- n)))
	nil))
(defun split (lst n)
  (list (left-n lst 3) (reverse (left-n (reverse lst) (- (length lst) n)))))
;q18
(defun r-list (lst n)
  (if (> n 1)
	(r-list (cdr lst) (1- n))
	lst))

(defun slice (lst start end)
  (reverse (r-list (reverse (r-list lst start)) (- (length lst) (1- end)))))

;q19
(defun r-left (lst)
  (reverse (cons (car lst) (reverse (cdr lst)))))

(defun r-right (lst)
  (reverse (r-left (reverse lst))))

(defun rotate-aux (lst n)
  (cond ((= n 0) lst)
		((> n 0) (rotate-aux (r-left lst) (1- n)))
		((< n 0) (rotate-aux (r-right lst) (1+ n)))))
;q20
(defun remove-at (lst n)
  (if (= n 1)
	(cdr lst)
	(cons (car lst) (remove-at (cdr lst) (1- n)))))
