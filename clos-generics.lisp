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
    :specializer-profile ,(specializer-profile compiler-generic)
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
    (let* ((method-combination
             (ensure-method-combination
              (or method-combination '(standard))))
           (apo (or apo
                  ;; first value returned is the required parameters
                  (alexandria:parse-ordinary-lambda-list lambda-list)))
           (gf (make-instance 'compiler-generic
                 :lambda-list lambda-list
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
       (setf (fdefinition ',name) ,(build-gf-form gf))))))

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

(defun expand-early-defmethod (name qualifiers lambda-list body)
  (multiple-value-bind (lambda-list specializers)
      (parse-method-lambda-list lambda-list)
    (let* ((generic-function (cross-clasp:gf-info name))
           (gfp (not (not generic-function)))
           (generic-function
             (if gfp
                 generic-function
                 (make-instance 'compiler-generic
                   :lambda-list lambda-list ; FIXME: adjust &key?
                   :apo (alexandria:parse-ordinary-lambda-list
                         lambda-list)
                   :method-combination (ensure-method-combination
                                        '(standard))
                   :method-class (cross-clasp:find-compiler-class
                                  'standard-method)
                   :declarations ()
                   :class (cross-clasp:find-compiler-class
                           'standard-generic-function))))
           (method (make-instance 'compiler-method
                     :gf generic-function
                     :lambda-list lambda-list
                     :specializers specializers
                     :qualifiers qualifiers))
           (gfg (gensym "GENERIC-FUNCTION")))
      `(progn
         (eval-when (:compile-toplevel)
           (note-method ,generic-function ,method))
         (let ((,gfg ,(if gfp
                          `(fdefinition ',name)
                          (build-gf-form generic-function))))
           ,@(unless gfp
               `((setf (fdefinition ',name) ,gfg)))
           (push ,(build-method-form method)
                 (with-early-accessors (standard-generic-function)
                   (%generic-function-methods ,gfg))))))))

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
                     :mclass (mclass method) :slot (slot method)
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
                     :mclass (mclass method) :slot (slot method)
                     :original method :effective-slot effective)))
        (push final (effective-writers direct))
        final))))

(defun find-method-form (method methods-var)
  (let* ((gf (gf method))
         (pos (position method (methods gf))))
    (assert pos)
    `(elt ,methods-var ,pos)))

(defun build-outcome (outcome methods-var fname)
  (let ((methods `(list ,@(loop for method in (methods outcome)
                                collect (find-method-form method methods-var)))))
    (if fname
        `(early-make-instance effective-method-outcome
           :methods ,methods
           :form ',(form outcome)
           :function #',fname)
        ;; must be an accessor.
        (let ((method (second (form outcome)))) ; (call-method #<method> ...)
          `(early-make-instance ,(etypecase method
                                   (effective-reader 'optimized-slot-reader)
                                   (effective-writer 'optimized-slot-writer))
             :methods ,methods
             :index ',(location (effective-slot method))
             :slot-name ',(name (effective-slot method))
             :class (find-class ',(name (sclass method))))))))

(defun call-history-form (gf-form call-history fdefs)
  (let ((methods (gensym "METHODS")))
    `(let ((methods (with-early-accessors (standard-generic-function)
                      (generic-function-methods ,gf-form))))
       (flet (,@(remove nil fdefs))
         (list
          ,@(loop for (classes . outcome) in call-history
                  for classforms = (loop for class in classes
                                         collect `(find-class ',(name class)))
                  for classvec = `(vector ,@classforms)
                  for fdef in fdefs
                  for outform = (build-outcome outcome methods (first fdef))
                  collect `(cons ,classvec ,outform)))))))

;; given a call history, output a list of function definitions for the outcomes,
;; or NIL if there isn't one (basically if it's an accessor).
;; a function definition is (name lambda-list . body).
;; I put this in with the thought of sharing between the discriminator and the
;; outcomes, but honestly that's a lot of work for little reason.
(defun history-outcome-fdefs (gf call-history)
  (loop with req = (required-parameters gf)
        with rest = (if (restp gf) (gensym "REST") nil)
        with ll = `(,@req ,@(if rest `(&rest ,rest) nil))
        for (_ . outcome) in call-history
        for form = (form outcome)
        collect (if (and (consp form)
                      (eq (first form) 'call-method)
                      (typep (first form) 'effective-accessor))
                    nil
                    `(,(gensym "OUTCOME") ,ll
                      (with-effective-method-parameters (,@req ,rest)
                        ,form)))))

(defmacro satiate (name &rest speclists)
  (let* ((gfun (cross-clasp:gf-info name))
         (call-history (call-history-from-speclists gfun speclists))
         (gfv (gensym (string name))) (chv (gensym "CALL-HISTORY"))
         (outcome-fdefs (history-outcome-fdefs gfun call-history))
         (ch-form (call-history-form gfv call-history outcome-fdefs)))
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
