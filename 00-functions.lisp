(defun double (x)
  (* x 2))
;; 0. build a function
;; 1. associate fn to symbol-name
;; does same thing as defun
(setf (symbol-function 'triple)
      (lambda (x) (* x 3)))
(triple 3) ; 9
;; #' ~ get function object from name
;; #'-ing a lambda expression is reduntant
;;; lambda returns the fn object itself
(setf a #'double)
;; symbol-function ~ symbol -> function object
(symbol-function 'double)
;; symbol-name ~ symbol -> value
(symbol-value 'a)
;; eval ~ get value
;; funcall ~ apply fn object to argument
(funcall (eval a) 4) ; 8
;; apply ~ apply fn object to arg list
(apply (eval a) '(4)) ; 8
