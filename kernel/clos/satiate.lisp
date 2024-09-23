(in-package #:clos)

;;; FIXME: Define in config, or at least elsewhere
(defconstant +where-tag-mask+      #b11000)
(defconstant +derivable-where-tag+ #b00000)
(defconstant +rack-where-tag+      #b01000)
(defconstant +wrapped-where-tag+   #b10000)
(defconstant +header-where-tag+    #b11000)
(defconstant +fixnum-tag+ 342)
(defconstant +single-float-tag+ 310)
(defconstant +character-tag+ 1582)
(defconstant +cons-tag+ 30)

(defmacro core::header-stamp-case (stamp derivable rack wrapped header)
  `(case (logand (ash ,stamp 2) ,+where-tag-mask+)
     (,+derivable-where-tag+ ,derivable)
     (,+rack-where-tag+ ,rack)
     (,+wrapped-where-tag+ ,wrapped)
     (,+header-where-tag+ ,header)))

(defun instance-stamp (object)
  ;; This is way dumber than the eventual dfuns, but we can take advantage
  ;; of one cheat - nothing we're satiating here wants a non-general.
  (cond
    ((core:generalp object)
     (let ((hstamp (core::header-stamp object)))
       (core::header-stamp-case hstamp
         (core::derivable-stamp object)
         (core::rack-stamp object)
         (core::wrapped-stamp object)
         hstamp)))
    ((consp object) +cons-tag+)
    ((core:fixnump object) +fixnum-tag+)
    ((core:single-float-p object) +single-float-tag+)
    ((characterp object) +character-tag+)
    (t (error "Unknown object ~s" object))))

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
