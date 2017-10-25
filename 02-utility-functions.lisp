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
