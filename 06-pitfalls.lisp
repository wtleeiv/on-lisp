;;;; variable capture

;; free variable
;;; a variable not explicitly bound in a expression
;; macro skeleton
;;; macro expansion w/o args
;; watch out for:
;;; free skeleton vars
;;; binding vars in same expression as args

;; use earmuffs *global* for global vars

;; use gensyms to prevent capture :)

;; don't worry if writing a macro takes multiple tries
;;; they're hard :)
;; you can start by writing it w/o paying attention to var capture,
;; then going back and replacing w/ gensyms

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop)) ; prevent multiple evaluation of stop
         ((> ,var ,gstop)) ; here
       ,@body)))

;;;; number of evaluations
;; macro arguments are forms, not values
;;; and can be evaluated more than once
;;;; usuall this is bad
;; solution: bind form evaluation to variable (gensym)
;;; use that where you would use argument

;;;; order of evaluation
;; fn args evaluated left -> right
;; good practice for macros to do the same

;;;; non-functional expanders :(
;; macro expansion should be purely functional
;;; only depend on the arguments passed to it
;; don't modify &rest parameters in particular
;;; they could be shared elsewhwere

;;;; recursion
;; compiling recursive macros can sometimes lead to an infinite loop
;;; fn works
(defun ntha (n lst)
  (if (= n 0)
      (car lst)
      (ntha (1- n) (cdr lst))))
;;; macro translation -> infinite compilation loop :(
;;;; recurses on form of n, not it's value
(defmacro nthb (n lst)
  `(if (= ,n 0)
       (car ,lst)
       (nthb (1- ,n) (cdr ,lst))))
;; solutions:
;;; recursive -> iterative
(defmacro nthc (n lst)
  (let ((nn (gensym))
        (lyst (gensym)))
    `(do ((,nn ,n (1- ,nn))
          (,lyst ,lst (cdr ,lyst)))
         ((= ,nn 0) (car ,lyst)))))
;;; macro and function
(defmacro nthd (n lst)
  `(labels ((nth-fn (n lst)
              (if (= n 0)
                  (car lst)
                  (nth-fn (1- n) (cdr lst)))))
     (nth-fn ,n ,lst)))

;; recursive expander -> non-recursive expansion
(defmacro orb (&rest args)
  (if (null args)
      nil
      (let ((sym (gensym)))
        `(let ((,sym ,(car args)))
           (if ,sym
               ,sym
               (orb ,@(cdr args)))))))
