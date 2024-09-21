(in-package #:clos)

(defun instance-stamp (object)
  ;; FIXME: no magic numbers, also inline this or do fancier dfun generation.
  (let ((hstamp (core::header-stamp object)))
    (case (logand (ash hstamp 2) 24)
      (0 (core::derivable-stamp object))
      (8 (core::rack-stamp object))
      (16 (core::wrapped-stamp object))
      (24 hstamp)))) ; header

;;; Minimum needed to call generic functions.
;;; May be an overestimate since debugging my way down to a
;;; truly minimum set sounds atrocious.
(satiate generic-function-methods (standard-generic-function))
(satiate generic-function-a-p-o-function (standard-generic-function))

(satiate method-specializers (standard-method)
         (standard-reader-method)
         (standard-writer-method))

(satiate class-precedence-list
         (standard-class) (funcallable-standard-class))

(satiate eql-specializer-p
         (eql-specializer) (standard-class) (funcallable-standard-class))
(satiate compute-applicable-methods-using-classes
         (standard-generic-function t))
(satiate specializer-accepts-p
         (standard-class t) (funcallable-standard-class t)
         (eql-specializer t))
