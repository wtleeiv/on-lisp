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

;; don't include if using ccl
#-ccl (defmacro memq (obj lst)
        `(member ,obj ,lst :test #'eq))

(defmacro mac (expr)
  "pretty-print macroexpansion"
  `(pprint (macroexpand-1 ',expr)))

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

;; used in condlet (below)

(defun condlet-binds (vars cl)
  (mapcar (lambda (bindform)
            (if (consp bindform)
                (cons (cdr (assoc (car bindform) vars))
                      (cdr bindform))))
          (cdr cl)))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))

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


;;; conditional evaluation

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

;; helper fn for >case
(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))

;; case is like a switch statement (static choices)
;; >case evals cases
;; (cond ((in cases1) (do stuff)) ((in cases2) (do other-stuff)))
(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar (lambda (cl) (>casex g cl))
                       clauses)))))


;;; iteration
;; simple

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

;; used in do-tuples/c
(defun dt-args (len rest src)
  (map0-n (lambda (m)
            (map1-n (lambda (n)
                      (let ((x (+ m n)))
                        (if (>= x len)
                            `(nth ,(- x len) ,src)
                            `(nth ,(1- x) ,rest))))
                    len))
          (- len 2)))

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

(defun shuffle (x y)
  (cond ((null y) x)
        ((null x) y)
        (t (list* (car x) (car y)
                  (shuffle (cdr x) (cdr y))))))

(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
         (syms (mapcar (lambda (p)
                         (mapcar (lambda (x)
                                   ;; unused lexical variable
                                   ;; don't care what x is, just make a gensym for it
                                   (declare (ignore x))
                                   (gensym))
                                 (mklist (car p))))
                       pairs)))
    (labels ((rec (ps ss)
               (if (null ps)
                   ;; setq all vars to gensym vals
                   `(setq ,@(mapcan
                             (lambda (p s)
                               (shuffle (mklist (car p))
                                        s))
                             pairs syms))
                   ;; bind pairs, nest recurse form
                   (let ((body (rec (cdr ps) (cdr ss))))
                     (let ((var/s (caar ps))
                           (expr (cadar ps)))
                       ;; single -> let, multiple -> mvb
                       (if (consp var/s)
                           `(multiple-value-bind ,(car ss) ,expr
                              ,body)
                           `(let ((,@(car ss) ,expr))
                              ,body)))))))
      (rec pairs syms))))

(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym))
        ;; used in initial prog bindings
        (temps (mapcar (lambda (b)
                         (if (listp (car b))
                             (mapcar (lambda (x)
                                       (declare (ignore x))
                                       (gensym))
                                     (car b))
                             (gensym)))
                       binds)))
    `(let ,(mappend #'mklist temps) ; declare temp vars
       ;; inital binding of temp vars
       (mvpsetq ,@(mapcan (lambda (b var)
                            (list var (cadr b)))
                          binds
                          temps))
       ;; bind vars to temps
       (prog ,(mapcar (lambda (b var) (list b var))
               (mappend #'mklist (mapcar #'car binds))
               (mappend #'mklist temps))
          ,label
          (if ,test
              (return (progn ,@result)))
          ,@body
          ;; rebind vars to step-forms
          (mvpsetq ,@(mapcan (lambda (b)
                               (if (third b)
                                   (list (car b)
                                         (third b))))
                             binds))
          (go ,label)))))
