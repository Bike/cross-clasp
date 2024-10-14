(in-package #:clos)

;;;; Dealing with method combination objects.
;;;; See also define-method-combination.lisp and effective-method.lisp.

(defclass method-combination (metaobject)
  ((name :initarg :name :accessor method-combination-name)
   ;; The "compiler" is somewhat misleadingly named; it's the function that
   ;; outputs the effective method form.
   ;; The "compiler" functions take two arguments, plus the lambda-list from
   ;; the define-method-combination. The first argument is the generic function
   ;; (used for the :generic-function option of D-M-C), the second is the sorted
   ;; list of applicable methods, and the rest are the method combination options.
   (compiler :initarg :compiler :accessor method-combination-compiler)
   (options :initarg :options :accessor method-combination-options)))

;;; Method combinations are stored in the global *method-combinations* hash
;;; table. (the standard method on) FIND-METHOD-COMBINATION ignores the gf,
;;; and makes a new METHOD-COMBINATION instance with the "compiler" looked
;;; up in the hash table, and the name and options.

(defparameter *method-combinations-lock*
  (mp:make-lock :name 'find-method-combination))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *method-combinations* (make-hash-table :size 32 :test 'eq)))

(defun search-method-combination (name)
  (mp:with-lock (*method-combinations-lock*)
    (gethash name *method-combinations*)))

(eval-when (:load-toplevel :execute)
  (defun install-method-combination (name function)
    (mp:with-lock (*method-combinations-lock*)
      (setf (gethash name *method-combinations*) function))
    name))
;;; This definition only used during build. It ignores the lock, since the build
;;; is single-threaded anyway.
(eval-when (:compile-toplevel)
  (defun install-method-combination (name function)
    (setf (gethash name *method-combinations*) function)))

(defun make-method-combination (name compiler options)
  (early-make-instance method-combination
                       :name name
                       :compiler compiler
                       :options options))

(defgeneric find-method-combination (generic-function
                                     method-combination-type-name
                                     method-combination-options))

(defmethod find-method-combination ((gf standard-generic-function)
                                    name options)
  (declare (ignore gf))
  (make-method-combination name
			   (or (search-method-combination name)
                               (error "~A does not name a method combination"
                                      name))
			   options))
