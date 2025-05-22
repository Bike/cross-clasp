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

(defun (setf documentation) (doc object doc-type)
  (declare (ignore object doc-type))
  doc)
