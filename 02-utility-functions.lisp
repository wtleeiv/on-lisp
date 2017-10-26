;; aside:
;;; (consp nil) => nil
;;; (listp nil) => 't

;; bad
(defun all-nicknames (names)
  (if (null names)
      nil
      (nconc (nicknames (car names))
             (all-nicknames (cdr names)))))
;; good
(mapcan #'nicknames people)

;; ok
(defun nearest-bookstores (towns)
  (if (null towns)
      nil
      (let ((stores (bookstores (car towns))))
        (if stores
            (values (car towns) shops)
            (nearest-bookstores (cdr towns))))))
;; util
(defun find2 (fn lst)
  "return first list element which returns something from fn and it's value"
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))
;; chillin'
(find2 #'bookstores towns)

;; use functional arguments to abstract patterns
;; brevity & efficiency => beautiful code

;;; tiny utils

;; (proclaim '(inline last1 singlep append1 conc1 mklist))
;;; inline utils //forspeed
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

;;; bigger utils

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x) ; clever
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

;; remove-if-not ~ like haskell's filter

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

;; NOTE: implement zip
;; (mapcar #'list ...) ~ zip

(defun my-zip (x y)
  (labels ((rec (x y acc)
             (if (and x y)
                 (rec (cdr x) (cdr y) (cons (list (car x) (car y)) acc))
                 (if x
                     (values (nreverse acc) x)
                     (values (nreverse acc) y)))))
    (rec x y nil)))

;; works for arbirtary number of args
(defun zip (&rest args)
  (apply #'mapcar #'list args))

;;; doubly-recursive functions on lists

;; recurse on car and cdr
;;; grok this pattern
(defun flatten (lst)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   ('t (rec (car x) (rec (cdr x) acc))))))
    (rec lst nil)))

;; my way
;;; recurse on cdr
;;;; mosts lists have cdrs, so this is will overflow sooner
(defun my-prune (fn lst)
  (labels ((rec (x)
             (cond
               ((null x) nil)
               ((atom x) (if (funcall fn x) nil x))
               ('t (let ((car-result (rec (car x))))
                     (if car-result
                         (cons car-result (rec (cdr x))) ; recursive here
                         (rec (cdr x))))))))
    (rec lst)))

;; pg's way
;;; recurse on car
;;;; few lists have nested cars, so this will handle longer lists
;; move along list, consing satisfying car leaves (in reverse order)
;;; recurse into car if car is list, setting new acc to nil
;; when reach end, nreverse acc to get items in order
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

;;; searching

(defun find2 (fn lst)
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


