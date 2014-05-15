;q14
; Duplicate the elements of a list.
;     Example:
;         * (dupli '(a b c c d))
;             (A A B B C C C C D D)
(defun dupli (lst)
  (if (null lst) 
	nil
	(cons (car lst) (cons (car lst) (dupli (cdr lst))))))
;q15
;Replicate the elements of a list a given number of times.
;    Example:
;        * (repli '(a b c) 3)
;            (A A A B B B C C C)
;
(defun n-rep (elem n)
  (if (zerop n) nil
	(cons elem (n-rep elem (1- n)))))

(defun repli (lst n)
  (if (null lst) nil
	(append (n-rep (car lst) n) (repli (cdr lst) n))))
;q16
;Drop every N'th element from a list.
;    Example:
;        * (drop '(a b c d e f g h i k) 3)
;            (A B D E G H K)
(defun drop-aux (lst n cnt)
  (labels ((be-dropped (cnt n) (if (= (mod (1+ cnt) n) 0) t nil)))
	(cond ((null lst) nil)
		  ((be-dropped cnt n) (drop-aux (cdr lst) n (1+ cnt)))
		  (t (cons (car lst) (drop-aux (cdr lst) n (1+ cnt)))))))

(defun drop (lst n)
  (drop-aux lst n 0))

;q17
; Split a list into two parts; the length of the first part is given.
;     Do not use any predefined predicates.
;
;         Example:
;             * (split '(a b c d e f g h i k) 3)
;                 ( (A B C) (D E F G H I K))
(defun left-n (lst n)
  (if (> n 0)
	(cons (car lst) (left-n (cdr lst) (1- n)))
	nil))
(defun split (lst n)
  (list (left-n lst 3) (reverse (left-n (reverse lst) (- (length lst) n)))))
;q18
; Extract a slice from a list.
;     Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
;
;         Example:
;             * (slice '(a b c d e f g h i k) 3 7)
;                 (C D E F G)
(defun r-list (lst n)
  (if (> n 1)
	(r-list (cdr lst) (1- n))
	lst))

(defun slice (lst start end)
  (reverse (r-list (reverse (r-list lst start)) (- (length lst) (1- end)))))

;q19
; Rotate a list N places to the left.
;     Examples:
;         * (rotate '(a b c d e f g h) 3)
;             (D E F G H A B C)
;
;                 * (rotate '(a b c d e f g h) -2)
;                     (G H A B C D E F)
;
;                         Hint: Use the predefined functions length and append, as well as the result of problem P17.
(defun r-left (lst)
  (reverse (cons (car lst) (reverse (cdr lst)))))

(defun r-right (lst)
  (reverse (r-left (reverse lst))))

(defun rotate-aux (lst n)
  (cond ((= n 0) lst)
		((> n 0) (rotate-aux (r-left lst) (1- n)))
		((< n 0) (rotate-aux (r-right lst) (1+ n)))))
;q20
;Remove the K'th element from a list.
;    Example:
;        * (remove-at '(a b c d) 2)
;            (A C D)
(defun remove-at (lst n)
  (if (= n 1)
	(cdr lst)
	(cons (car lst) (remove-at (cdr lst) (1- n)))))
