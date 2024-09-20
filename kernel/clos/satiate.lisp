(in-package #:clos)

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
