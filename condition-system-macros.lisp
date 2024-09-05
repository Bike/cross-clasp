(in-package #:cross-clasp)

(defmacro %check-type (place type &optional type-string)
  "Args: (check-type place typespec [string-form])
Signals a continuable error, if the value of PLACE is not of the specified
type.  Before continuing, receives a new value of PLACE from the user and
checks the type again.  Repeats this process until the value of PLACE becomes
of the specified type.  STRING-FORM, if given, is evaluated only once and the
value is used to indicate the expected type in the error message."
  (when (and (consp type) (eq 'quote (car type)))
    (error "Quoted type specifier in ~s: ~s"
           'check-type type))
  (let ((aux (gensym)))
    `(let ((,aux ,place))
       (unless (typep ,aux ',type)
	 (setf ,place (do-check-type ,aux ',type ',type-string ',place)))
       nil)))
