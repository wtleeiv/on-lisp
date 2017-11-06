;; variable capture

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
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))
