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

;; pg. 87
