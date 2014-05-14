;p08
;Eliminate consecutive duplicates of list elements.
;    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
;
;        Example:
;            * (compress '(a a a a b c c a a d e e e e))
;                (A B C A D E)
(defun compress-aux (lst res)
  (cond ((null res) (compress-aux (cdr lst) (cons (car lst) res)))
		((null lst) (reverse res))
		((eq (car lst) (car res)) (compress-aux (cdr lst) res))
		(t (compress-aux (cdr lst) (cons (car lst) res)))))
(defun compress (lst)
  (compress-aux lst nil))
;p09
;Pack consecutive duplicates of list elements into sublists.
;    If a list contains repeated elements they should be placed in separate sublists.
;
;        Example:
;            * (pack '(a a a a b c c a a d e e e e))
;                ((A A A A) (B) (C C) (A A) (D) (E E E E))
(defun attach-dup-item (item lst)
  (let ((elem (car lst)))
	  (cons (cons item elem) (cdr lst))))

(defun pack-aux (lst res)
  (cond ((null res) (pack-aux (cdr lst) (cons (list (car lst)) res)))
		((null lst) (reverse res))
		((eq (car lst) (caar res)) (pack-aux (cdr lst) (attach-dup-item (car lst) res)))
		(t (pack-aux (cdr lst) (cons (list (car lst)) res)))))
(defun pack (lst)
  (pack-aux lst nil))
;p10
;Run-length encoding of a list.
;    Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
;
;        Example:
;            * (encode '(a a a a b c c a a d e e e e))
;                ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
(defun encode (lst)
  (labels ((count-elem (elem) (list (length elem) (car elem))))
	(mapcar #'count-elem (pack lst))))
