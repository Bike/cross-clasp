(in-package #:clos)

(defclass eql-specializer (specializer)
  ((object :initarg :object :reader eql-specializer-object)))

(defvar *eql-specializer-lock* (mp:make-lock :name 'eql-specializer))

(defvar *eql-specializer-hash*
  (make-hash-table :size 128 :test #'eql))

(defun intern-eql-specializer (object)
  (mp:with-lock (*eql-specializer-lock*)
    (or (gethash object *eql-specializer-hash*)
      (setf (gethash object *eql-specializer-hash*)
            (early-make-instance eql-specializer :object object)))))
