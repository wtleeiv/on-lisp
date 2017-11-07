;; macros...classic
;;; create context, conditional eval, multiple eval

;; creating context
(defmacro my-let (binds &body body)
  `((lambda ,(mapcar (lambda (bind) ; get bind vars
                       (if (consp bind) (car bind) bind))
              binds)
      ,@body) ; lambda body is the body passed in
    ,@(mapcar (lambda (bind) ; get bind values
                (if (consp bind) (cadr bind) nil))
              binds)))

(defmacro when-bind ((var expr) &body body)
  "bind expr to val, if val is non-nil, eval body"
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  "bind all exprs, eval body if all binds are non-nil"
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
         (if ,(caar binds)
             (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (sym) `(,sym (gensym)))
          syms)
     ,@body))
