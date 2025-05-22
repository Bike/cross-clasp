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

(defun (setf documentation) (doc object doc-type)
  (declare (ignore object doc-type))
  doc)
