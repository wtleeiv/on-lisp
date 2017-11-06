;;;; me

(defmacro with-gensyms ((&rest gensyms) &body body)
  "let for gensym bindings"
  `(let ,(loop for gs in gensyms collect `(,gs (gensym)))
       ,@body))

;; TODO: make more haskell-esque
(defun seq (x n)
  (labels ((iter (n acc)
             (if (= n 0)
                 acc
                 (iter (1- n) (cons x acc)))))
    (iter n nil)))

;;;; pg

;;; small

(proclaim '(inline last1 singlep append1 conc1 mklist))
;;; inline these //forspeed

(defun last1 (lst)
  (car (last lst)))

(defun singlep (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst atom)
  (append lst (list atom)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;;; bigger

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x) ; clever
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  "filter based on fn return vals"
  (let ((acc nil))
    (dolist (x lst (nreverse acc))
      (let ((val (funcall fn x)))
        (if val (push val acc))))))

(defun group (lst n)
  (if (<= n 0) (error (format nil "invalid length ~a" n)))
  (labels ((rec (lst acc)
             (let ((rest (nthcdr n lst)))
               (if rest
                   (rec rest (cons (subseq lst 0 n) acc))
                   (if (= (length lst) n)
                       (values (nreverse (cons lst acc)) nil)
                       (values (nreverse acc) lst))))))
    (if lst (rec lst nil) nil)))

;;; doubly-recursive

(defun flatten (lst)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   ('t (rec (car x) (rec (cdr x) acc))))))
    (rec lst nil)))

(defun prune (fn lst)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc))) ; recursive here
                   ('t (rec (cdr tree)
                            (if (funcall fn (car tree))
                                acc
                                (cons (car tree) acc)))))))
    (rec lst nil)))

;;; search

(defun find2 (fn lst)
  "return first list element which returns something from fn and it's value"
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst ; not nil
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x lst :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test)) :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((seq lst (cdr seq)))
        ((or (null seq) (funcall fn (car seq)))
         (values (nreverse acc) seq))
      (push (car seq) acc))))

;;; comparative search

(defun most (fn lst)
  "returns first maximum"
  (if (null lst)
      (values nil nil)
      (let* ((winner (car lst))
             (max (funcall fn winner)))
        (dolist (obj (cdr lst) (values winner max))
          (let ((val (funcall fn obj)))
            (when (> val max)
              (setf winner obj max val)))))))

(defun best (fn lst)
  "takes comparison fn and list, returns winner"
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst) wins)
          (if (funcall fn obj wins)
              (setf wins obj))))))

(defun mostn (fn lst)
  "returns all maximums"
  (if (null lst)
      (values nil nil)
      (let* ((result (list (car lst)))
             (max (funcall fn (car result))))
        (dolist (obj (cdr lst) (values (nreverse result) max))
          (let ((val (funcall fn obj)))
            (cond ((> val max)
                   (setf result (list obj) max val))
                  ((eql val max)
                   (push obj result))))))))

;;; mapping

(defun map-> (fn start end-fn step-fn)
  (do ((i start (funcall step-fn i))
       (result nil))
      ((funcall end-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mapa-b (fn a b &optional (step 1))
  (map-> fn a (lambda (n) (> n b)) (lambda (n) (+ n step))))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mappend (fn &rest lsts)
  "non-destructive mapcan"
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts (nreverse result))
      (dolist (obj lst)
        (push (funcall fn obj) result)))))

;; if args are atoms, apply to fn
;; else, mapcar over args w/ recursion
(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             (lambda (&rest args)
               (apply #'rmapcar fn args))
             args)))


;;; IO
;; prolly not used much

(defun readlist (&rest args)
  "make list from string"
  (values (read-from-string (concatenate 'string "(" (apply #'read-line args) ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))

;;; symbols & strings

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list (lambda (c)
               (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

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


;;; macros

(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro mac (expr)
  "pretty-print macroexpansion"
  `(pprint (macroexpand-1 ',expr)))

(defmacro when-bind ((var expr) &body body)
  "evaluate 'when' after binding test variable"
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop)) ; prevent multiple evaluation of stop
         ((> ,var ,gstop)) ; here
       ,@body)))
