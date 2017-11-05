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






(defun make-initforms (bindforms)
  (mapcar (lambda (b)
            (if (consp b)
                (list (car b) (cadr b))
                (list (car b) nil)))
          bindforms))
(defun make-stepforms (bindforms)
  (mapcan (lambda (b)
            (if (and (consp b) (third b))
                (list (car b) (third b))
                nil))
          bindforms))

