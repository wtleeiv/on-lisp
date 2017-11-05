;;;; Macros, finally!

;; whenever you see the expression: (nil! x)
;;; change it to (setq x nil) before evaluating
(defmacro nil! (var)
  "set arg to nil"
  (list 'setq var nil))

;; functions return results
;; macros return expressions
;;; which, when evaluated, return results

;; when lisp sees macro
;;; list w/ macro symbol as first element
;; lisp macroexpands into expression, then evaluates

;; macros are applied to the *expressions* passed to them
;;; not the values of expressions passed to them

;; parser: source code -> lists of lisp objects
;; macros: lists of lisp objects -> manipulated lists of lisp objects
;;; intermediate program manipulation
;; compiler: lisp objects -> machine code

;; a macro is a lisp function, that returns lisp expressions!

(defmacro nil-backquote (var)
  `(setq ,var nil))

;;; backquote
;; equivalent to list with elements quoted
;;; (list 'a 'b 'c)
;; comma and comma-at turn off commenting (quote and splice)

(defmacro nif (expr pos zero neg)
  "contitional based on sign of expr"
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

;; ugly af
(defmacro nif-no-backquote (expr pos zero neg)
  (list 'case
        (list 'truncate (list 'signum expr))
        (list 1 pos)
        (list 0 zero)
        (liist -1 neg)))

(defmacro my-when (test &body body)
  `(if ,test (progn ,@body)))

;; &body and &rest are identical,
;;; except for pretty printing

;; backquote can be used anywhere you want to generate a list
;;; not only in macros
(defun greet (name)
  `(hello ,name))


;;; simple macros

(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro mac (expr)
  "pretty-print macroexpansion"
  `(pprint (macroexpand-1 ',expr)))

;; destructuring bind: pattern match variable binding
;;; (destructuring-bind (x (y) . z) '(a (b) c d)
;;;   (list x y z)) -> (a b (c d))

;; note: '(a b . (c d)) -> (a b c d)

;; explicit destructuring useful in defining macros
;;; nested parameter list

(defmacro when-bind ((var expr) &body body)
  "evaluate 'when' after binding test variable"
  `(let ((,var ,expr))
     (when ,var
       ,@body)))


;; *how to write macros*
;;; 1. how do you want to use it?
;;; 2. what code should it generate?
;;; 3. write code to transform 1 -> 2

;;;; do
;;; use:
;; (do ((w 3)
;;      (x 1 (1+ x))
;;      (y 2 (1+ y))
;;      (z))
;;     ((> x 10) (princ z) y)
;;   (princ x)
;;   (princ y))
;;; expand:
;; (prog ((w 3) (x 1) (y 2) (z nil)) ; set vars
;;  foo ; label
;;    (if (> x 10) ; if test, return forms
;;        (return (progn (princ z) y)))
;;    (princ x) ; body
;;    (princ y)
;;    (psetq x (1+ x) y (1+ y)) ; update vars
;;    (go foo)) ; goto label
(defmacro my-do (bindforms (test &rest result) &body body)
  (flet ((make-initforms (bindforms)
           (mapcar (lambda (b)
                     (if (consp b)
                         (list (car b) (cadr b))
                         (list (car b) nil)))
                   bindforms))
         (make-stepforms (bindforms)
           (mapcan (lambda (b)
                     (if (and (consp b) (third b))
                         (list (car b) (third b))
                         nil))
                   bindforms)))
    (let ((label (gensym)))
      `(prog ,(make-initforms bindforms)
          ,label
          (if ,test
              (return (progn ,@result)))
          ,@body
          (psetq ,@(make-stepforms bindforms))
          (go ,label)))))

;; macro code
;;; expander code
;;;; used to generate expansion code
;;;; clarity / efficiency
;;; expansion code
;;;; the code that will be run
;;;; efficiency / clarity

;; code you wouldn't usually write (goto, setq, etc)
;; is more ok in macros (ex. see above)
;;; write macros to hide unsafe code
;;; use it programatically, not raw

;; be mindful of macro redifinition
;;; code that sues macros don't automatically get new def
;;; because macro is expanded, and reference to it is lost

;; macro rules
;;; define macros before they are called
;;; when redefining macros, redefine all callers

;;;; symbol macros
;;; like macros that take no args
;;; sometimes useful
;; (symbol-macrolet ((hi (progn (print "Howdy!")
;;                              1)))
;;   (+ hi 2))
;; ->
;; "Howdy!"
;; 3

;; macros control evaluation of their arguments

;; macro vs function
(defun avg (&rest args)
  (/ (apply #'+ args) (length args)))
;; compute length at compile time
(defmacro avg (&rest args)
  `(/ ,(+ ,@args) ,(length args)))

;; macros emcapsulate patterns
;;; allow program to declare in a clearer voice what it's doing

;; example: graphics language
;;; pre-macro
(defun move-objs (objs dx dy)
  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
    (dolist (o objs)
      (incf (obj-x o) dx)
      (incf (obj-y o) dy))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
              (max x1 xb) (max y1 yb)))))
(defun scale-objs (objs factor)
  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
    (dolist (o objs)
      (setf (obj-dx o) (* (obj-dx o) factor))
      (setf (obj-dy o) (* (obj-dy o) factor)))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
              (max x1 xb) (max y1 yb)))))
;;; post-macro
(defmacro with-redraw ((var objs) &body body)
  (let ((gob (gensym))
        (x0 (gensym)) (y0 (gensym))
        (x1 (gensym)) (y1 (gensym)))
    `(let ((,gob ,objs))
       (multiple-value-bind (,x0 ,y0 ,x1 ,y1) (bounds ,gob)
         (dolist (,var ,gob)
           ,@body)
         (multiple-value-bind (xa ya xb yb) (bounds ,gob)
           (redraw (min ,x0 xa) (min ,y0 ya)
                   (max ,x1 xb) (max ,y1 yb)))))))
(defun move-objs (objs dx dy)
  (with-redraw (o objs)
    (incf (obj-x o) dx)
    (incf (obj-y o) dy)))
(defun scale-objs (objs factor)
  (with-redraw (o objs)
    (setf (obj-dx o) (* (obj-dx o) factor))
    (setf (obj-dy o (* (obj-dy o) factor)))))
