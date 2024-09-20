(in-package #:cross-clasp.clasp.clos)

(defgeneric build-method-initargs (compiler-method)
  (:method-combination append))

(defmethod build-method-initargs append ((method compiler-method))
  `(:generic-function (fdefinition
                       ',(name (gf compiler-method)))
    :lambda-list ',(lambda-list compiler-method)
    :specializers (list ,@(mapcar #'specializer-form
                                  (specializers compiler-method)))
    :qualifiers ',(qualifiers compiler-method)))

(defmethod build-method-initargs append ((method compiler-accessor))
  `(:slot-definition ,(slot method)))
(defmethod build-method-initargs append ((method effective-accessor))
  `(:original ,(original method)
    :location ,(location (effective-slot method))))

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:client)
                                                (object compiler-method)
                                                &optional env)
  (declare (ignore env))
  (values `(early-allocate-instance ,(name (mclass object)))
          `(early-initialize-instance ,(name (mclass object))
                                      ,method
                                      ,@(build-method-initargs object))))

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:client)
                                                (object outcome)
                                                &optional env)
  (declare (ignore env))
  (let ((form (form object))
        (ep (and (consp form) (eq (first form) 'call-method)
              (typep (second form) 'effective-accessor)))
        (oclass (cond ((not ep) 'effective-method-outcomeg)
                      ((typep (second form) 'effective-reader)
                       'optimized-slot-reader)
                      ((typep (second form) 'effective-writer)
                       'optimized-slot-writer)
                      (t (error "Unknown method class in ~s" form)))))
    (values `(early-allocate-instance ,oclass)
            `(early-initialize-instance
              ,oclass ,object
              :methods '(,@(methods object))
              ,@(if ep
                    `(:index ,(location (slot (second form)))
                      :slot-name ',(name (slot (second form)))
                      :class ,(ecase (second form)
                                (effective-reader
                                 (first (specializers (second form))))
                                (effective-writer
                                 (second (specializers (second form))))))
                    `(:function ,(let* ((gf (gf (first (methods object))))
                                        (req (required-parameters gf))
                                        (rest (if (restp gf)
                                                  (gensym "REST")
                                                  nil))
                                        (ll (if rest
                                                `(,@req &rest ,rest)
                                                req)))
                                   `(lambda ,ll
                                      (with-effective-method-parameters
                                          (,@req ,rest)
                                        ,(form object))))
                      :form ',form))))))

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:client)
                                                (object compiler-class)
                                                &optional env)
  (declare (ignore env))
  `(find-class ',(name object)))

;;; method dumping is based on the premises that
;;; a) by the time they appear literally, they will already be loaded into the
;;;    generic function, so we just need to grab them
;;; b) EXCEPT for effective accessors, which will not yet exist. But their
;;;    direct slots will exist, so they must be installed.
;;; c) any one effective accessor will only be dumped in one file. Otherwise
;;;    we might get duplicates, which I think is harmless but dumb.
;;; d) slots also already exist.

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:client)
                                                (object compiler-method)
                                                &optional env)
  (let ((gf (gf object))
        (pos (position object (methods gf))))
    (assert pos () "Method on ~s for ~s ~s does not exist"
            (name gf) (qualifiers object) (mapcar #'name (specializers gf)))
    `(with-early-accessors (standard-generic-function)
       (elt (generic-function-methods (fdefinition ',(name gf))) ,pos))))

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:client)
                                                (object effective-accessor)
                                                &optional env)
  (multiple-value-bind (mclass class fmaker cache)
      (etypecase object
        (effective-reader (values 'effective-reader-method
                                  (first (specializers object))
                                  'std-reader-method-function
                                  '%effective-readers))
        (effective-writer (values 'effective-writer-method
                                  (second (specializers object))
                                  'std-writer-method-function
                                  '%effective-writers)))
    (let* ((original (original object))
           ;; we dump the pos form ourselves rather than rely on m-l-f
           ;; because we need to look up the slotd in its class, and the
           ;; slotd doesn't know what its class is.
           (dslot (slot original))
           (dslotpos (position dslot (direct-slots class))))
      (assert dslotpos () "Slot ~s is not present in its class ~s"
              (name dslot) (name class))
      (values `(early-allocate-instance ,mclass)
              `(progn
                 (early-initialize-instance ,mclass ,object
                   :function (,fmaker ',(name direct))
                   :generic-function (fdefinition ',(name (gf object)))
                   :lambda-list ',(lambda-list object)
                   :qualifiers ',(qualifiers object)
                   :specializers ',(specializers object)
                   :original ',original
                   :location ',(location (effective-slot object)))
                 (with-early-accessors (std-class direct-slot-definition)
                   (push ,object (,cache (elt (class-direct-slots ,class)
                                              ,dslotpos)))))))))
