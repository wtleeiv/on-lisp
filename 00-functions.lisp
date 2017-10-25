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

(defun ty-remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
          (ty-remove-if fn (cdr lst))
          (cons (car lst) (ty-remove-if fn (cdr lst))))))

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-count () (setf counter 0)))

(defun make-adder (n)
  (lambda (x) (+ x n)))

(defun make-two-adder (n)
  (lambda (x y) (+ x y n)))

(defmacro fun-call (fn &rest params)
  `(funcall (eval ,fn) ,@params))

(defun slow-count (n)
  (labels ((iter (n c)
             (if (eq n 0)
                 c
                 (iter
                  (1- n)
                  (+ n c)))))
    (iter n 0)))

(proclaim '(optimize speed)) ; tail-call recursion
(defun fast-count (n)
  (labels ((iter (n c)
             (declare (type fixnum n c))
             (if (eq n 0)
                 c
                 (iter
                  (the fixnum (1- n))
                  (the fixnum (+ n c))))))
    (iter n 0)))
