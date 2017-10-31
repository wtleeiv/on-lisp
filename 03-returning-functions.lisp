;; examples of returning functions

(defun my-complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))

(defun joiner (obj)
  (typecase obj
    (cons #'append)
    (number #'+)))

(defun join (&rest args)
  (apply (joiner (car args)) args))

(defun make-adder (n)
  #'(lambda (x) (+ n x)))

;;; memoizer

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

;;; composition f(g(x))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

;; example
(defun complement-again (pred)
  (compose #'not pred))

;;; logic function builders

(defun fif (if then &optional else)
  (lambda (&rest args)
    (if (apply if args)
        (apply then args)
        (if else (apply else args)))))

(defun fint (fn &rest fns)
  "intersection of funcitons"
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (&rest args)
            (and (apply fn args) (apply chain args) )))))

(defun fun (fn &rest fns)
  "union of functions"
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (&rest args)
            (or (apply fn args) (apply chain args) )))))

;;; recursion on cdrs

(defun lrec (rec &optional base)
  "rec: fn of two args (car and recursor)"
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          (lambda () (self (cdr lst)))))))
    #'self))

;; (setf my-length (lrec (lambda (x f) (1+ (funcall f))) 0))
