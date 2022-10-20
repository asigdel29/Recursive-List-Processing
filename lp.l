(defun singleton-p (lst)
 (cond
  ((null lst) nil)
  ((null (cdr lst) ) t)
  (t nil)
 )
)

(defun rac (lst)
 (cond 
  ((singleton-p lst) (car lst))
  (t (rac (cdr lst)))
 )
)

(defun rdc (lst)
 (cond 
  ((singleton-p lst) ())
  ((cons (car lst) (rdc (cdr lst))))
 )
)

(defun snoc (x lst)
 (cond 
  ((null lst) (list x))
  (t (cons (car lst) (snoc x (cdr lst))))
 )
)

(defun palindrome-p (lst) 
 (cond
  ((null lst) t)
  ((singleton-p lst) t)
  ((equal (car lst) (rac lst))
   (palindrome-p (cdr (rdc lst)))
  )
  (t nil)
 )
)

(defun select (lst i)
 (cond 
  ((< i 0) nil)
  ((> i (length lst)) nil)
  ((equal i 0) (car lst))
  (t (select (cdr lst) (- i 1)))
 )
)
 
(defun pick (lst)
 (select lst (random (length lst)))
)

(defun sum (nl)
 (cond
  ((equal (length nl) 0) 0)
  ((+ (car nl) (sum (cdr nl))))
 )
)

(defun product (nl)
 (cond
  ((null nl) 1)
  ((* (car nl) (product (cdr nl))))
 )
)

(defun iota (n)
 (cond
  ((equal n 0) ())
  (t (snoc n (iota (- n 1))))
 )
)

(defun duplicate (n lo)
 (cond
  ((equal n 0) ())
  ((< n 0) nil)
  (t (cons lo (duplicate (- n 1) lo)))
 )
)

(defun factorial (n)
 (product (iota n))
)

(defun power (number exponent)
 (product (duplicate exponent number))
)

(defun filter-in (pred lst)
 (cond
  ((null lst) ())
  ((apply pred (list (car lst))) (cons (car lst) (filter-in pred (cdr lst))))
  (t (filter-in pred (cdr lst)))
 )
)

(defun filter-out (pred lst)
 (cond
  ((null lst) ())
  ((apply pred (list (car lst))) (filter-out pred (cdr lst)))
  (t (cons (car lst) (filter-out pred (cdr lst))))
 )
)

(defun take-from (obj lst)
 (cond
  ((null lst) ())
  ((equal obj (car lst)) (take-from obj (cdr lst)))
  (t (cons (car lst) (take-from obj (cdr lst))))
 )
)

(defun take-one (obj lst)
 (cond
  ((null lst) ())
  ((equal obj (car lst)) (cdr lst))
  (t (cons (car lst) (take-one obj (cdr lst))))
 )
)

(defun random-permutation (x &aux e)
 (setf e (pick x))
 (cond
  ((null x) ())
  ((singleton-p x) x)
  (t (cons e (random-permutation (take-one e x))))
 )
)
