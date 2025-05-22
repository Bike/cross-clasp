(in-package #:core)

(defun (setf documentation) (doc object doc-type)
  (declare (ignore object doc-type))
  doc)
