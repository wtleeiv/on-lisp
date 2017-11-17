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
                     choices)))))

;; case is like a switch statement (static choices)
;; >case evals cases
;; (cond ((in cases1) (do stuff)) ((in cases2) (do other-stuff)))
(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar (lambda (cl) (>casex g cl))
                       clauses)))))

(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))


;;; iteration
;; eval forms more than once

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro till (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop)) ; prevent multiple evaluation of stop
         ((> ,var ,gstop)) ; here
       ,@body)))

;;; more complex iteration

;; mapcar takes #lists eq to #args
;;; and recurses down all til reach end of one
;; call mapc (don't save returns) on n cdr lists of source
(defmacro do-tuples/o (parms source &body body)
  (if parms ; don't do if no params passed in
      (let ((src (gensym)))
        `(prog ((,src ,source))
            (mapc (lambda ,parms ,@body)
                  ,@(map0-n (lambda (n)
                              `(nthcdr ,n ,src))
                            (1- (length parms))))))))

;; wraps source to cover all elements
;; uses a bunch of nths to get args
;;; complex math, lots of off-by-ones possible
(defmacro do-tuples/c (parms source &body body)
  (if parms
      (with-gensyms (src rest bodfn)
        (let ((len (length parms)))
          `(let ((,src ,source))
             (when (nthcdr ,(1- len) ,src) ; source len >= parms len
               (labels ((,bodfn ,parms ,@body)) ; defun here
                 (do ((,rest ,src (cdr ,rest)))
                     ((not (nthcdr ,(1- len) ,rest)) ; when wrap req'd
                      ;; *at* or past end
                      ,@(mapcar (lambda (args) `(,bodfn ,@args))
                                (dt-args len rest src))
                      nil) ; do above, then return nil
                   ;; (map1-n (1- n) len) same as (map0-n n (- len 1))
                   (,bodfn ,@(map1-n (lambda (n)
                                       `(nth ,(1- n) ,rest))
                                     len))))))))))

(defun dt-args (len rest src)
  (map0-n (lambda (m)
            (map1-n (lambda (n)
                      (let ((x (+ m n)))
                        (if (>= x len)
                            `(nth ,(- x len) ,src)
                            `(nth ,(1- x) ,rest))))
                    len))
          (- len 2)))


;;; multiple value iteration

;; rebinds initially contains all bindforms
;; recurse and cons together rebinding form
(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3) ; no step-form
         (mvdo-rebind-gen (cdr rebinds))) ; skip this parm
        (t (cons (list (if (atom (caar rebinds))
                           'setq
                           'multiple-value-setq)
                       (caar rebinds) ; var/s
                       (third (car rebinds))) ; step-form
                 (mvdo-rebind-gen (cdr rebinds))))))

;; recurse through bindings, execute body, call rebind-gen
(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      ;; fininshed all bindings
      (let ((label (gensym))) ; block name
        `(prog nil ; create block
            ,label ; tag code
            (if ,(car test) ; end-test
                (return (progn ,@(cdr test)))) ; return w/ value
            ,@body ; execute body
            ;; why is this req'd if null initial bindings?
            ,@(mvdo-rebind-gen rebinds) ; rebind parms
            (go ,label))) ; loop (goto label)
      ;; bind parms recursively (nested let/mvb), eval rec-form (above)
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
        (let ((var/s (caar binds)) (expr (cadar binds)))
          (if (atom var/s)
              `(let ((,var/s ,expr)) ,rec)
              `(multiple-value-bind ,var/s ,expr ,rec))))))

(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))
list*

(defun shuffle (x y)
  (cond ((null y) x)
        ((null x) y)
        (t (list* (car x) (car y)
                  (shuffle (cdr x) (cdr y))))))

(defmacro mvpsetq (&rest args)
  )
