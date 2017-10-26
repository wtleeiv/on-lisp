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