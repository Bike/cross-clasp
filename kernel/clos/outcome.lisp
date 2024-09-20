(in-package "CLOS")

;;; An "outcome" is a potential "outcome" of a generic function call.
;;; Basically, an outcome represents an effective method function,
;;; only it's simpler in many cases.

;;; Outcomes

(defclass outcome ()
  ((%methods :initarg :methods :reader outcome-methods)))
(defclass optimized-slot-accessor (outcome)
  ((%index :initarg :index :reader optimized-slot-accessor-index)
   (%slot-name :initarg :slot-name :reader optimized-slot-accessor-slot-name)
   (%class :initarg :class :reader optimized-slot-accessor-class)))
(defclass optimized-slot-reader (optimized-slot-accessor) ())
(defclass optimized-slot-writer (optimized-slot-accessor) ())
(defclass effective-method-outcome (outcome)
  ((%form :initarg :form :reader effective-method-outcome-form)
   (%function :initarg :function :reader effective-method-outcome-function)))

(defun outcome= (outcome1 outcome2)
  (eq outcome1 outcome2)) ; thanks, caching! (in find-existing-outcome)
