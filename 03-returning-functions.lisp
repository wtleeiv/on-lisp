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

;; not tail-recursive
;;; good for prototyping, but not optimized in many cases
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

;; length: (lrec (lambda (x f) (1+ (funcall f))) 0)
;; copy-list: (lrec (lambda (x f) (cons x (funcall f))))
;; remove-duplicates: (lrec (lambda (x f) (adjoin x (funcall f))))
;; find-if, for function fn: (lrec (lambda (x f) (if (fn x) x (funcall f))))
;; some, for function fn: (lrec (lambda (x f) (or (fn x) (funcall f))))
;; every, for function fn: (lrec (lambda (x f) (and (fn x) (funcall f))) t)

(defun my-copy-tree (tree)
  (if (atom tree)
      tree
      (cons (my-copy-tree (car tree))
            (if (cdr tree) (my-copy-tree (cdr tree))))))

;;; tree recursion

(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec
                          (self (car tree))
                          (if (cdr tree) (self (cdr tree)))))))
    #'self))

;; coppy-tree (ttrav #'cons)
;; count-leaves (ttrav (lambda (l r) (+ l (or r 1))) 1)
;; flatten (ttrav #'nconc #'mklist)


(defun trec (rec &optional (base #'identity))
  "rec takes 3 args: obj, car (left) rec, & cdr (right) rec"
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec tree
                          (lambda () (self (car tree)))
                          (lambda () (if (cdr tree)
                                         (self (cdr tree))))))))
    #'self))

;; rfind-if (trec (lambda (o l r) (or (funcall l) (funcall r))) (lambda (obj) (fn obj)))

;; #. read macro ~ construct function at readtime (if args are defined)
;;; (find-if #.(compose #'oddp #'truncate) lst)
