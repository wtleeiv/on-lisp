;; rotatef ~ rotates element values
;;; two elements -> swaps them
(defun bad-reverse (lst)
  (let* ((len (length lst))
         (mid (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i mid)
         lst)
      (rotatef (nth i lst) (nth j lst)))))

(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

;; functional code flows

(defun stack ()
  (let ((m-stack ()))
    (values
     (lambda (x) (push x m-stack)) ; push
     (lambda () (pop m-stack)) ; pop
     (lambda () (null m-stack))))) ; empty

(defmacro with-gensyms ((&rest gensyms) &body body)
  `(let ,(loop for gs in gensyms collect `(,gs (gensym)))
       ,@body))

(defmacro new-stack (name)
  `(with-gensyms (a b c)
     (multiple-value-bind (a b c) (stack)
       (setf (symbol-function (intern (format nil "~a~a" ,name '-push))) a
             (symbol-function (intern (format nil "~a~a" ,name '-pop))) b
             (symbol-function (intern (format nil "~a~a" ,name '-is-empty))) c))))

