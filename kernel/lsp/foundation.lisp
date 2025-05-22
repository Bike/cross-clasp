(in-package #:core)
#+(or)
(defconstant lambda-list-keywords
  (if (boundp 'lambda-list-keywords)
      (symbol-value 'lambda-list-keywords)
      '(&ALLOW-OTHER-KEYS
        &AUX &BODY &ENVIRONMENT &KEY
        &OPTIONAL &REST
        &VA-REST
        &WHOLE)))

(defun 1- (num) (- num 1))
(defun 1+ (num) (+ num 1))

(defun constantly (object)
  (lambda (&rest arguments) (declare (ignore arguments)) object))

(defmacro cons-car (list) `(car (the cons ,list)))
(defmacro cons-cdr (list) `(cdr (the cons ,list)))

;;
;;   "Return true if OBJECT is the same as some tail of LIST, otherwise false."
;;
(defun tailp (object list)
  (if (null list)
      (null object)
    (do ((list list (cdr list)))
        ((atom (cdr list)) (or (eql object list) (eql object (cdr list))))
      (if (eql object list)
          (return t)))))
;;
;;   "Return a copy of LIST before the part which is the same as OBJECT."
;;

;;; Definition from CLHS 14.2.30 (LDIFF, TAILP)
(defun ldiff (list object)
  (unless (listp list)
    (error 'simple-type-error
           :format-control "Not a proper list or a dotted list.; ~s."
           :format-arguments (list list)
           :datum list
           :expected-type 'list))
  (do ((list list (cdr list))
       (r '() (cons (car list) r)))
      ((atom list)
       (if (eql list object) (nreverse r) (nreconc r list)))
    (when (eql object list)
      (return (nreverse r)))))

(defmacro apply-key (key element)
  `(if ,key
       (funcall ,key ,element)
       ,element))

;;   "Add ITEM to LIST unless it is already a member."
(defun adjoin (item list &key key (test #'eql) test-not)
  (when test-not
    (setq test (complement test-not)))
  (if (member (apply-key key item) list :key key :test test)
      list
    (cons item list)))

(defun hash-table-iterator (hash-table)
  (let ((pairs (core:hash-table-pairs hash-table))
        (hash-index 0))
    (function (lambda ()
      (if (>= hash-index (length pairs))
          nil
          (let* ((key (elt pairs hash-index))
                 (val (elt pairs (incf hash-index))))
            (incf hash-index)
            (values t key val)))))))

;   "Substitute data of ALIST for subtrees matching keys of ALIST."
(defun sublis (alist tree &key key (test #'eql) test-not)
  (when test-not
    (setq test (complement test-not)))
  (labels ((sub (subtree)
                (let ((assoc (assoc (apply-key key subtree) alist :test test)))
                  (cond
                   (assoc (cdr assoc))
                   ((atom subtree) subtree)
                   (t (let ((car (sub (car subtree)))
                            (cdr (sub (cdr subtree))))
                        (if (and (eq car (car subtree)) (eq cdr (cdr subtree)))
                            subtree
                          (cons car cdr))))))))
    (sub tree)))
;   "Substitute data of ALIST for subtrees matching keys of ALIST destructively."
(defun nsublis (alist tree &key key (test #'eql) test-not)
  (when test-not
    (setq test (complement test-not)))
  (labels ((sub (subtree)
                (let ((assoc (assoc (apply-key key subtree) alist :test test)))
                  (cond
                   (assoc (cdr assoc))
                   ((atom subtree) subtree)
                   (t
                    (rplaca subtree (sub (car subtree)))
                    (rplacd subtree (sub (cdr subtree)))
                    subtree)))))
    (sub tree)))


(defun invoke-unix-debugger ()
  (gdb "invoking unix debugger"))

(defun (setf documentation) (doc object doc-type)
  (declare (ignore object doc-type))
  doc)

(defun class-name (x)
  (core:name-of-class x))

(defvar *defun-inline-hook* nil)
