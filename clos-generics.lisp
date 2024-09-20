(in-package #:cross-clasp.clasp.clos)

(defclass outcome ()
  ((%methods :initarg :methods :reader methods)
   (%form :initarg :form :reader form)))

(defun parse-defgeneric-options (options)
  (loop with apo with mc with doc with gfclass with mclass
        for (name . value) on options
        if (eq name :method)
          collect value into methods
        if (eq name 'declare)
          append value into declarations
        if (member name seen-options)
          do (error "Redundant ~s" name)
        collect name into seen-options
        if (eq name :argument-precedence-order)
          do (setf apo value)
        else if (eq name :method-combination)
               do (setf mc value)
        else if (eq name :documentation)
               ;; if this was for user code we'd want to do more
               ;; validation here (check cdr is null)
               do (setf doc (first value))
        else if (eq name :generic-function-class)
               do (setf gfclass (first value))
        else if (eq name :method-class)
               do (setf mclass (first value))
        else do (error "Unknown option ~s" name)
        finally (return (values methods apo declarations doc
                                mc gfclass mclass))))

(defun build-gf-form (compiler-generic)
  `(early-make-instance
    ,(name (gf-class compiler-generic))
    :lambda-list ',(lambda-list compiler-generic)
    :method-combination nil ; installed later
    :argument-precedence-order ',(apo compiler-generic)
    :method-class (find-class
                   ',(name (method-class compiler-generic)))
    :declarations ',(declarations compiler-generic)))

(defmacro early-defgeneric (name lambda-list &rest options)
  (multiple-value-bind (methods apo declarations doc
                        method-combination class method-class)
      (parse-defgeneric-options options)
    (declare (ignore doc))
    (unless (null methods)
      (error ":method not yet supported"))
    (multiple-value-bind (required optional rest keys aokp aux keysp)
        (alexandria:parse-ordinary-lambda-list lambda-list)
      (declare (ignore keys aokp aux))
      (let* ((restp (or optional rest keysp))
             (method-combination
               (ensure-method-combination
                (or method-combination '(standard))))
             (apo (or apo required))
             (gf (make-instance 'compiler-generic
                   :name name
                   :lambda-list lambda-list
                   :reqargs required :restp restp
                   :apo apo
                   :method-combination method-combination
                   :method-class (cross-clasp:find-compiler-class
                                  (or method-class 'standard-method))
                   :declarations declarations
                   :class (cross-clasp:find-compiler-class
                           (or class 'standard-generic-function)))))
        `(progn
           (eval-when (:compile-toplevel)
             (note-generic ',name ,gf))
           (setf (fdefinition ',name) ,(build-gf-form gf)))))))

;;; return the unspecialized lambda list and the specializer specs.
(defun parse-method-lambda-list (lambda-list)
  (multiple-value-bind (req opt rest keys aokp aux keyp)
      (alexandria:parse-ordinary-lambda-list lambda-list
                                             :allow-specializers t)
    (values (nconc (mapcar (lambda (r)
                             (if (consp r) (first r) r))
                           req)
                   (unless (null opt) (list '&optional))
                   (loop for o in opt
                         if (third o) ; suppliedp
                           collect o
                         else
                           collect (list (first o) (second o)))
                   (when rest (list '&rest rest))
                   (when keyp (list '&key))
                   (loop for k in keys
                         if (third k)
                           collect k
                         else
                           collect (list (first k) (second k)))
                   (when aokp (list '&allow-other-keys))
                   (when aux (list '&aux))
                   (copy-list aux))
            (mapcar (lambda (r) (if (consp r) (second r) 't)) req))))

(defun specializer-form (specializer)
  (etypecase specializer
    (compiler-class `(find-class ',(name specializer)))))

(defgeneric build-method-initargs (compiler-method)
  (:method-combination append))

(defmethod build-method-initargs append ((method compiler-method))
  `(:generic-function (fdefinition ',(name (gf method)))
    :lambda-list ',(lambda-list method)
    :specializers (list ,@(mapcar #'specializer-form
                                  (specializers method)))
    :qualifiers ',(qualifiers method)))

(defun slot-form (class slot)
  (let ((pos (position slot (slots class))))
    (assert pos)
    `(with-early-accessors (std-class)
       (nth ,pos (class-direct-slots (find-class ',(name class)))))))

(defmethod build-method-initargs append ((method compiler-reader))
  `(:slot-definition ,(slot-form (first (specializers method)) (slot method))))
(defmethod build-method-initargs append ((method compiler-writer))
  `(:slot-definition ,(slot-form (second (specializers method)) (slot method))))

(defun build-method-form (compiler-method)
  `(early-make-instance ,(name (mclass compiler-method))
                        ,@(build-method-initargs compiler-method)))

(defun block-name (function-name)
  (etypecase function-name
    (symbol function-name)
    ((cons (eql setf) (cons symbol null)) (second function-name))))

(defun expand-early-defmethod (name qualifiers lambda-list body)
  (multiple-value-bind (lambda-list specializers)
      (parse-method-lambda-list lambda-list)
    (let* ((generic-function (cross-clasp:gf-info name))
           (gfp (not (not generic-function)))
           (generic-function
             (if gfp
                 generic-function
                 (multiple-value-bind (required optional rest
                                       keys aokp aux keysp)
                     (alexandria:parse-ordinary-lambda-list lambda-list)
                   (declare (ignore keys aokp aux))
                   (make-instance 'compiler-generic
                     :name name
                     :lambda-list lambda-list ; FIXME: adjust &key?
                     :reqargs required :restp (or optional rest keysp)
                     :apo required
                     :method-combination (ensure-method-combination
                                          '(standard))
                     :method-class (cross-clasp:find-compiler-class
                                    'standard-method)
                     :declarations ()
                     :class (cross-clasp:find-compiler-class
                             'standard-generic-function)))))
           (mfname (gensym "METHOD"))
           (method (make-instance 'compiler-method
                     :gf generic-function
                     :lambda-list lambda-list
                     :specializers (mapcar #'cross-clasp:find-compiler-class
                                           specializers)
                     :qualifiers qualifiers
                     :class (method-class generic-function)
                     :contf-form `#',mfname))
           (gfg (gensym "GENERIC-FUNCTION")))
      `(progn
         (eval-when (:compile-toplevel)
           (note-method ,generic-function ,method))
         ,(multiple-value-bind (body decls)
              (alexandria:parse-body body :documentation t)
            `(defun ,mfname (,@lambda-list)
               ,@decls
               (block ,(block-name name) ,@body)))
         (let ((,gfg ,(if gfp
                          `(fdefinition ',name)
                          (build-gf-form generic-function))))
           ,@(unless gfp
               `((setf (fdefinition ',name) ,gfg)))
           (with-early-accessors (standard-generic-function)
             (push ,(build-method-form method)
                   (%generic-function-methods ,gfg))))))))

(defmacro early-defmethod (name &rest rest)
  (loop for r on rest
        for e = (first rest)
        if (consp e)
          return (expand-early-defmethod name qualifiers e (rest r))
        else collect e into qualifiers))

;;;

(defun apo-permutation (generic)
  (loop with req = (required-parameters generic)
        for a in (apo generic)
        collect (or (position a req) (error "Invalid APO"))))

(defun apo-function (generic)
  ;; Return a function that permutes a list according to the APO.
  (let ((perm (apo-permutation generic)))
    (lambda (list)
      (loop for p in perm collect (nth p list)))))

(defun applicable-method-p (method classes)
  (loop for spec in (specializers method)
        for class in classes
        always (member spec (class-precedence-list class))))

(defun method< (method1 method2 gspecs apof)
  (loop for spec1 in (funcall apof (specializers method1))
        for spec2 in (funcall apof (specializers method2))
        for gspec in (funcall apof gspecs)
        do (case (specializer< spec1 spec2 gspec)
             ((<) (return t))
             ((>) (return nil)))))

(defun specializer< (s1 s2 gspec)
  (let ((cpl (class-precedence-list gspec)))
    (cond ((eq s1 s2) '=)
          ((member s1 (member s2 cpl)) '>)
          ((member s2 (member s1 cpl)) '<)
          (t (error "Incomparable specializers: ~s ~s" s1 s2)))))

(defun compute-applicable-methods-using-classes (generic classes)
  (sort (loop for method in (methods generic)
              when (applicable-method-p method classes)
                collect method)
        (let ((apof (apo-function generic)))
          (lambda (m1 m2) (method< m1 m2 classes apof)))))

;;;

(defun call-history-from-speclists (gfun speclists)
  (loop for speclist in speclists
        for classes = (mapcar #'cross-clasp:find-compiler-class speclist)
        for am = (compute-applicable-methods-using-classes gfun classes)
        for fm = (final-methods am classes)
        for existing = (find fm outcomes :key #'methods)
        for outcome = (or existing
                        (make-instance 'outcome
                          :methods fm
                          :form (compute-effective-method gfun fm)))
        collect (cons classes outcome) into call-history
        unless existing
          collect outcome into outcomes
        finally (return call-history)))

(defun final-methods (methods classes)
  (loop for method in methods collect (final-method method classes)))

(defgeneric final-method (method classes))
(defmethod final-method ((method compiler-method) classes)
  (declare (ignore classes))
  method)
(defmethod final-method ((method compiler-reader) classes)
  (let* ((direct (slot method))
         (class (first classes))
         (effective (find (name direct) (slots class) :key #'name)))
    (or (find effective (effective-readers direct) :key #'effective-slot)
      (let ((final (make-instance 'effective-reader
                     :gf (gf method) :lambda-list (lambda-list method)
                     :specializers classes :qualifiers (qualifiers method)
                     :class (mclass method)
                     :original method :effective-slot effective)))
        (push final (effective-readers direct))
        final))))
(defmethod final-method ((method compiler-writer) classes)
  (let* ((direct (slot method))
         (class (second classes))
         (effective (find (name direct) (slots class) :key #'name)))
    (or (find effective (effective-writers direct) :key #'effective-slot)
      (let ((final (make-instance 'effective-writer
                     :gf (gf method) :lambda-list (lambda-list method)
                     :specializers classes :qualifiers (qualifiers method)
                     :class (mclass method)
                     :original method :effective-slot effective)))
        (push final (effective-writers direct))
        final))))

(defun find-method-form (method methods-var)
  (let* ((gf (gf method))
         (pos (position method (methods gf))))
    (assert pos)
    `(elt ,methods-var ,pos)))

(defun call-history-form (call-history)
  `(list
    ,@(loop for (classes . outcome) in call-history
            for classforms = (loop for class in classes
                                   collect `(find-class ',(name class)))
            for classvec = `(vector ,@classforms)
            collect `(cons ,classvec ,outcome))))

(defmacro satiate (name &rest speclists)
  (let* ((gfun (cross-clasp:gf-info name))
         (call-history (call-history-from-speclists gfun speclists))
         (gfv (gensym (string name))) (chv (gensym "CALL-HISTORY"))
         (ch-form (call-history-form call-history)))
    `(with-early-accessors (standard-generic-function)
       (let* ((,gfv (fdefinition ',name))
              (,chv ,ch-form))
         (setf (generic-function-call-history ,gfv) ,chv
               ;; the SP has been updated by defmethods, so set again.
               (generic-function-specializer-profile ,gfv)
               ,(specializer-profile gfun))
         (set-funcallable-instance-function
          ,gfv
          ,(generate-discriminator gfun gfv call-history))
         (values)))))
