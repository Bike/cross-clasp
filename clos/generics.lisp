(in-package #:cross-clasp.clasp.clos)

(defclass outcome ()
  ((%methods :initarg :methods :reader methods)
   (%form :initarg :form :reader form)))

(defun parse-defgeneric-options (options)
  (loop with apo with mc with doc with gfclass with mclass
        for (name . value) in options
        if (member name seen-options)
          do (error "Redundant ~s" name)
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
        else if (eq name :method)
               collect value into methods
        else if (eq name 'declare)
               append value into declarations
        else do (error "Unknown option ~s" name)
        unless (member name '(declare :method))
          collect name into seen-options
        finally (return (values methods apo declarations doc
                                mc gfclass mclass))))

(defun build-mc-form (method-combination)
  `(early-make-instance method-combination
                        :name ',(name method-combination)
                        :compiler (search-method-combination
                                   ',(name method-combination))
                        :options ',(options method-combination)))

(defun build-gf-form (compiler-generic)
  `(let ((gf (early-make-instance
              ,(name (gf-class compiler-generic))
              :lambda-list ',(lambda-list compiler-generic)
              :method-combination ,(build-mc-form
                                    (gf-method-combination compiler-generic))
              :argument-precedence-order ',(apo compiler-generic)
              :method-class (find-class
                             ',(name (method-class compiler-generic)))
              specializer-profile ',(specializer-profile compiler-generic)
              :declarations ',(declarations compiler-generic))))
     (core:setf-function-name gf ',(name compiler-generic))
     (core:setf-lambda-list gf ',(lambda-list compiler-generic))
     (set-funcallable-instance-function
      ;; this is invalidated-discriminator-closure, but that's defined later.
      ;; miss is defined in miss.lisp.
      gf (lambda (&rest args)
           (declare (core:lambda-name invalidated-discriminator))
           (apply #'miss gf args)))
     gf))

(defmacro early-defgeneric (name lambda-list &rest options)
  (multiple-value-bind (methods apo declarations doc
                        method-combination class method-class)
      (parse-defgeneric-options options)
    (declare (ignore doc))
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
           (setf (fdefinition ',name) ,(build-gf-form gf))
           ,@(loop for method in methods
                   collect `(defmethod ,name ,@method)))))))

;;; return the parsed lambda list, but the second value is the list of
;;; specializer specifiers.
(defun parse-method-lambda-list (lambda-list)
  (multiple-value-bind (req opt rest keys aokp aux keyp)
      (alexandria:parse-ordinary-lambda-list lambda-list
                                             :allow-specializers t)
    (multiple-value-bind (req specs)
        (loop for r in req
              if (consp r)
                collect (first r) into params
                and collect (second r) into specs
              else
                collect r into params
                and collect 't into specs
              finally (return (values params specs)))
      (values req specs opt rest keys aokp aux keyp))))

;;; Given a parsed lambda list, reconstruct it.
;;; This is used after getting the specializers out.
(defun reconstruct-lambda-list (required optional rest keys aokp aux keyp)
  `(,@required
    ,@(when optional '(&optional)) ,@(loop for opt in optional
                                           for (var default -p) = opt
                                           collect (if -p
                                                       opt
                                                       (list var default)))
    ,@(when rest `(&rest ,rest))
    ,@(when keyp '(&key)) ,@(loop for key in keys
                                  for ((keyword var) default -p) = key
                                  collect (if -p
                                              key
                                              (list (list keyword var) default)))
    ;; Keyword checking is done by the GF, so methods should not check again
    ,@(when (or keyp aokp) '(&allow-other-keys))
    ,@(when aux `(&aux ,@aux))))

(defun specializer-form (specializer)
  (etypecase specializer
    (compiler-class `(find-class ',(name specializer)))
    (eql-specializer `(intern-eql-specializer
                       ',(mop:eql-specializer-object specializer)))))

(defgeneric build-method-initargs (compiler-method)
  (:method-combination append))

(defmethod build-method-initargs append ((method compiler-method))
  `(:generic-function (fdefinition ',(name (gf method)))
    :lambda-list ',(lambda-list method)
    :keywords ',(method-keywords method)
    :aok-p ',(method-allows-other-keys-p method)
    :specializers (list ,@(mapcar #'specializer-form
                                  (specializers method)))
    :qualifiers ',(qualifiers method)
    :function ,(second (method-function method))))

(defun direct-slot-form (class slot)
  (let ((pos (position slot (mop:class-direct-slots class))))
    (assert pos)
    `(with-early-accessors (std-class)
       (nth ,pos (class-direct-slots (find-class ',(name class)))))))

(defmethod build-method-initargs append ((method compiler-reader))
  `(:slot-definition
    ,(direct-slot-form (first (specializers method)) (slot method))))
(defmethod build-method-initargs append ((method compiler-writer))
  `(:slot-definition
    ,(direct-slot-form (second (specializers method)) (slot method))))

(defun build-method-form (compiler-method)
  `(early-make-instance ,(name (mclass compiler-method))
                        ,@(build-method-initargs compiler-method)))

(defun block-name (function-name)
  (etypecase function-name
    (symbol function-name)
    ((cons (eql setf) (cons symbol null)) (second function-name))))

(defun parse-specializer (spec)
  (etypecase spec
    (symbol (cross-clasp:find-compiler-class spec))
    ((cons (eql eql) (cons (cons (eql quote) (cons t null)) null))
     (intern-eql-specializer (second (second spec))))
    ((cons (eql eql) (cons t null))
     (error "Can't handle evaluated EQL specializer: ~a" spec))))

(defun expand-early-defmethod (name qualifiers lambda-list body)
  (multiple-value-bind (required specializers optional rest keys aokp aux keysp)
      (parse-method-lambda-list lambda-list)
    (let* ((generic-function (cross-clasp:gf-info name))
           (gfp (not (not generic-function)))
           (restp (or optional rest keysp))
           (generic-function
             (if gfp
                 generic-function
                 (make-instance 'compiler-generic
                   :name name
                   :lambda-list lambda-list ; FIXME: adjust &key?
                   :reqargs required :restp restp
                   :apo required
                   :method-combination (ensure-method-combination
                                        '(standard))
                   :method-class (cross-clasp:find-compiler-class
                                  'standard-method)
                   :declarations ()
                   :class (cross-clasp:find-compiler-class
                           'standard-generic-function))))
           (mfsname (format nil "~a~@[-~a~]-(~{~a~^ ~})-METHOD"
                            name qualifiers specializers))
           (package
             ;; We can't define stuff in the CL package
             ;; so we just substitute CLOS for it. But we don't always want
             ;; to use CLOS since we define methods from other packages and
             ;; CLOS is locked.
             (let ((s (etypecase name
                        (symbol (symbol-package name))
                        ((cons (eql setf) (cons symbol null))
                         (symbol-package (second name))))))
               (if (eq s (load-time-value (find-package "CL") t))
                   (find-package "CROSS-CLASP.CLASP.CLOS")
                   s)))
           (mfname (intern mfsname package))
           (cname (gensym "CONTINUATION"))
           (argsname (gensym "METHOD-ARGS"))
           (method (make-instance 'compiler-method
                     :gf generic-function
                     :lambda-list lambda-list
                     :keywords (mapcar #'caar keys)
                     :aok-p aokp
                     :specializers (mapcar #'parse-specializer specializers)
                     :qualifiers qualifiers
                     :class (method-class generic-function)
                     :function-form `(contf (make-%contf-method-function #',mfname) #',mfname)))
           (gfg (gensym "GENERIC-FUNCTION")))
      `(progn
         (eval-when (:compile-toplevel)
           ,@(unless gfp
               `((note-generic ',name ,generic-function)))
           (note-method ,generic-function ,method))
         ,(multiple-value-bind (body decls)
              (alexandria:parse-body body :documentation t)
            `(defun ,mfname (,cname &rest ,argsname)
               (block ,(block-name name)
                 (flet (;; If you want to use next-method-p: Too bad
                        (call-next-method (&rest args)
                          (if args
                              (apply ,cname args)
                              (apply ,cname ,argsname))))
                   (declare (ignorable #'call-next-method))
                   (apply (lambda ,(reconstruct-lambda-list
                                    required optional rest keys aokp aux keysp)
                            (declare (ignorable ,@required))
                            ,@decls
                            ,@body)
                          ,argsname)))))
         (let ((,gfg ,(if gfp
                          `(fdefinition ',name)
                          (build-gf-form generic-function))))
           ,@(unless gfp
               `((setf (fdefinition ',name) ,gfg)))
           (with-early-accessors (standard-generic-function)
             ;; If we specialized anything, update the specializer profile.
             ,@(loop with tc = (cross-clasp:find-compiler-class 't)
                     for spec in (specializers method)
                     for i from 0
                     if (typep spec 'eql-specializer)
                       collect `(aref sp ,i) into body
                       and collect `(let ((e (aref sp ,i))
                                          (o ',(mop:eql-specializer-object spec)))
                                      (if (listp e) (cons o e) (list o)))
                             into body
                     else unless (eq spec tc)
                            collect `(aref sp ,i) into body
                     ;; FIXME: this won't work if there was already an eql spec
                            and collect t into body
                     finally (return
                               (if (null body)
                                   nil
                                   `((let ((sp (generic-function-specializer-profile ,gfg)))
                                       (setf ,@body))))))
             (push ,(build-method-form method)
                   (%generic-function-methods ,gfg))))))))

(defmacro early-defmethod (name &rest rest)
  (loop for r on rest
        for e = (first r)
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
        always (member spec (mop:class-precedence-list class))))

(defun method< (method1 method2 gspecs apof)
  (loop for spec1 in (funcall apof (specializers method1))
        for spec2 in (funcall apof (specializers method2))
        for gspec in (funcall apof gspecs)
        do (case (specializer< spec1 spec2 gspec)
             ((<) (return t))
             ((>) (return nil)))))

(defun specializer< (s1 s2 gspec)
  (let ((cpl (mop:class-precedence-list gspec)))
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
        for existing = (find fm outcomes :key #'methods :test #'equal)
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
         (effloc (location (find (name direct) (mop:class-slots class) :key #'name))))
    (or (find effloc (effective-readers direct) :key #'location)
      (let ((final (make-instance 'effective-reader
                     :gf (gf method) :lambda-list (lambda-list method)
                     :specializers classes :qualifiers (qualifiers method)
                     :class (mclass method)
                     :original method :location effloc)))
        (push final (effective-readers direct))
        final))))
(defmethod final-method ((method compiler-writer) classes)
  (let* ((direct (slot method))
         (class (second classes))
         (effloc (location (find (name direct) (mop:class-slots class) :key #'name))))
    (or (find effloc (effective-writers direct) :key #'location)
      (let ((final (make-instance 'effective-writer
                     :gf (gf method) :lambda-list (lambda-list method)
                     :specializers classes :qualifiers (qualifiers method)
                     :class (mclass method)
                     :original method :location effloc)))
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
         (gfv (gensym (string (if (consp name) (second name) name))))
         (chv (gensym "CALL-HISTORY"))
         (ch-form (call-history-form call-history)))
    `(with-early-accessors (standard-generic-function)
       (let* ((,gfv (fdefinition ',name))
              (,chv ,ch-form))
         (setf (generic-function-call-history ,gfv) ,chv)
         (set-funcallable-instance-function
          ,gfv
          ,(generate-discriminator gfun gfv call-history))
         (values)))))
