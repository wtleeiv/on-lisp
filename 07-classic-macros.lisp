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

(defmacro condlet (clauses &body body)
  "bind vars conditionally, then eval body with bindings"
  (let ((bodfn (gensym))
        (vars (mapcar (lambda (v) (cons v (gensym))) ; bind gensyms to non-duplicate vars
                      (remove-duplicates
                       (mapcar #'car ; get var names
                               (mappend #'cdr clauses)))))) ; get all var bindings
    `(labels ((,bodfn ,(mapcar #'car vars)
                ,@body))
       (cond ,@(mapcar (lambda (cl) (condlet-clause vars cl bodfn))
                       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar (lambda (bindform)
            (if (consp bindform)
                (cons (cdr (assoc (car bindform) vars))
                      (cdr bindform))))
          (cdr cl)))


;; the 'with-' macro
;;; deal w/ state

;; unwind-protect
;;; eval all subsequent forms regardless if 1st form throws error

;; as macros grow in complexity, its simpler to combine with fns
;;; splice body into lambda, pass to fn
;; but that's lame :)

;; conditional evaluation
;; pg 150

(defmacro nif (expr pos zero neg)
  (let ((val (gensym))) ; declare gensyms outside of backquote
    `(let ((,val ,expr)) ; get new gensym at compile-time -> replace gensym val in expanded code
       (cond ((plusp ,val) ,pos)
             ((zerop ,val) ,zero)
             (t ,neg)))))

(defmacro in (obj &rest choices)
  "short-circuit test if obj is in choices"
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar (lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro inq (obj &rest choices)
  "quote choices, eval obj"
  `(in ,obj ,@(mapcar (lambda (c) `',c) choices)))

(defmacro in-if (fn &rest choices)
  "short-circuit test-fn applied to choices"
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar (lambda (c) `(funcall ,fnsym ,c))
                     ,choices)))))

;; case is like a switch statement (static choices)
;; >case evals cases
;; (cond ((in cases1) (do stuff)) ((in cases2) (do other-stuff)))
(defmacro >case (expr &rest clauses)
  )
