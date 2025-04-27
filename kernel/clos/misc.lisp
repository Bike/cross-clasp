(in-package "CLOS")

;;; find-method-combination

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

;;; no-applicable-method

(defgeneric no-applicable-method (generic-function &rest arguments))

(defmethod no-applicable-method (gf &rest args)
  (error 'no-applicable-method-error :generic-function gf :arguments args))

;;; function-keywords

(defgeneric function-keywords (method))
(defmethod function-keywords ((method standard-method))
  (values (method-keywords method) (method-allows-other-keys-p method)))

;;; generic-function-name

(defgeneric generic-function-name (generic-function))
(defmethod generic-function-name ((gf standard-generic-function))
  (core:function-name gf))
