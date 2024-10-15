(in-package #:clos)

;;; This works on both class locations (conses) and instance ones.
(defun standard-location-access (instance location)
  (if (core:fixnump location)
      (core:rack-ref (core:instance-rack instance) location)
      (car location)))

(defun (setf standard-location-access) (val instance location)
  (if (core:fixnump location)
      (setf (core:rack-ref (core:instance-rack instance) location) val)
      (setf (car location) val)))

(defun slot-value (object slot-name)
  (let* ((class (class-of object))
         (slotd
           ;; FIND not defined yet.
           (loop for prospect in (class-slots class)
                 for prospect-name = (slot-definition-name prospect)
                 when (eql prospect-name slot-name)
                   return prospect)))
    (if slotd
        (slot-value-using-class class object slotd)
        ;; Only the primary value of SLOT-MISSING is returned.
        (values (slot-missing class object slot-name 'slot-value)))))

(defun (setf slot-value) (value object slot-name)
  (let* ((class (class-of object))
         (slotd
           (loop for prospect in (class-slots class)
                 for prospect-name = (slot-definition-name prospect)
                 when (eql prospect-name slot-name)
                   return prospect)))
    (if slotd
        (setf (slot-value-using-class class object slotd) value)
        (slot-missing class object slot-name 'setf value)))
  ;; 7.7.12: value of slot-missing is ignored for setf.
  value)

(defgeneric slot-value-using-class (class object slot-definition))
(defgeneric (setf slot-value-using-class) (value class object slot-definition))
(defgeneric slot-boundp-using-class (class object slot-definition))
(defgeneric slot-makunbound-using-class (class object slot-definition))

(defmethod slot-value-using-class ((class std-class) object slotd)
  (let* ((location (slot-definition-location slotd))
         (value (standard-location-access object location)))
    (if (core:sl-boundp value)
        value
        (values (slot-unbound class object (slot-definition-name slotd))))))

(defmethod (setf slot-value-using-class) (value (class std-class) object slotd)
  (setf (standard-location-access object (slot-definition-location slotd)) value))

(defmethod slot-boundp-using-class (class object slotd)
  (core:sl-boundp (standard-location-access object (slot-definition-location slotd))))

(defmethod slot-makunbound-using-class (class object slotd)
  (setf (standard-location-access object (slot-definition-location slotd))
        (core:unbound)))
