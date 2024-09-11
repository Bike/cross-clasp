(in-package #:cross-clasp.clasp.clos)

(defclass compiler-class ()
  ((%name :initarg :name :reader name)
   (%supers :initarg :supers :reader direct-superclasses)
   (%subs :initform nil :accessor direct-subclasses)
   (%class-precedence-list :accessor class-precedence-list)
   (%direct-slots :initarg :direct-slots :reader direct-slots)
   (%slots :accessor slots)
   (%direct-default-initargs :initarg :direct-default-initargs
                             :reader direct-default-initargs)
   (%default-initargs :accessor default-initargs)
   (%metaclass :initarg :metaclass :reader metaclass)))

;;; for debugging
(defmethod print-object ((o compiler-class) stream)
  (print-unreadable-object (o stream :type t)
    (write (name o) :stream stream))
  o)

(defclass compiler-slotd ()
  ((%name :initarg :name :reader name)
   (%initform :initarg :initform :reader initform)
   (%initformp :initarg :initformp :reader initformp :type boolean)
   (%initargs :initarg :initargs :initform () :reader initargs)
   (%readers :initarg :readers :initform () :reader readers)
   (%writers :initarg :writers :initform () :reader writers)
   (%type :initarg :type :initform t :reader stype)
   (%location :initarg :location :initform nil :reader location)))

;;; also for debugging
(defmethod print-object ((o compiler-slotd) stream)
  (print-unreadable-object (o stream :type t)
    (write (name o) :stream stream))
  o)

(defun parse-slot-specifier (slot-specifier)
  (etypecase slot-specifier
    (symbol (values slot-specifier nil nil nil nil nil t nil))
    (cons (loop with name = (first slot-specifier)
                with initargs = ()
                with initform
                with initformp = nil
                with readers = ()
                with writers = ()
                with type = t with typep = nil
                with location = nil
                for (key value) on (rest slot-specifier) by #'cddr
                do (ecase key
                     ((:initform)
                      (if initformp
                          (error "duplicate initform")
                          (setf initform value initformp t)))
                     ((:initarg)
                      (check-type value symbol "a valid initarg")
                      (push value initargs))
                     ((:reader)
                      (check-type value symbol "a valid reader name")
                      (push value readers))
                     ((:writer)
                      (check-type value
                                  (or symbol
                                    (cons (eql setf) (cons symbol null)))
                                  "a valid writer name")
                      (push value writers))
                     ((:accessor)
                      (check-type value symbol "a valid accessor name")
                      (push value readers)
                      (push `(setf ,value) writers))
                     ((:type)
                      (if typep
                          (error "duplicate type")
                          (setf type value typep t)))
                     ((:location)
                      (check-type value (integer 0) "a slot location")
                      (if location
                          (error "duplicate location")
                          (setf location value))))
                finally (return (values name
                                        initform initformp initargs
                                        readers writers type
                                        location))))))

(defun make-compiler-slotd (slot-specifier)
  (multiple-value-bind (name initform initformp initargs
                        readers writers type location)
      (parse-slot-specifier slot-specifier)
    (make-instance 'compiler-slotd
      :name name :initform initform :initformp initformp
      :initargs initargs :readers readers :writers writers
      :type type :location location)))

(defun parse-class-options (options)
  (loop with metaclass with documentation
        with default-initargs with default-initargs-p
        for (key . value) in options
        do (ecase key
             ((:metaclass)
              (if metaclass
                  (error "Duplicate ~s option" :metaclass)
                  (destructuring-bind (clname) value
                    (setf metaclass clname))))
             ((:documentation)
              (if documentation
                  (error "Duplicate ~s option" :documentation)
                  (destructuring-bind (docstring) value
                    (setf documentation docstring))))
             ((:default-initargs)
              (if default-initargs-p
                  (error "Duplicate ~s option" :default-initargs)
                  (setf default-initargs value
                        default-initargs-p t))))
        finally (return (values metaclass
                                documentation default-initargs))))

(defun compute-effective-slot (slotds location)
  (flet ((app (reader)
           (remove-duplicates
            (mapcan (lambda (x) (copy-list (funcall reader x))) slotds)
            ;; EQUAL for setf writer names
            ;; i mean this rem-dup might be pointless anyway
            :test #'equal)))
    (multiple-value-bind (initform initformp)
        (loop for slotd in slotds
              when (initformp slotd)
                return (values (initform slotd) t)
              finally (return (values nil nil)))
      ;; Verify location
      (loop with locp = nil
            for slotd in slotds
            for loc = (location slotd)
            when loc
              do (cond (locp
                        (error "Duplicate ~s" :location))
                       ((= loc location))
                       (t (error "Location mismatch for ~s"
                                 (name (first slotds)))))
                 (setf locp t))
      ;; Make the slotd
      (make-instance 'compiler-slotd
        :name (name (first slotds)) :initform initform :initformp initformp
        :initargs (app #'initargs)
        :readers (app #'readers) :writers (app #'writers)
        :type `(and ,@(loop for slotd in slotds
                            for type = (stype slotd)
                            unless (eq type 't) ; who care
                              collect type))
        :location location))))

(defun compute-slots (cpl)
  (let* (;; An alist from slot names to lists of direct slotds
         ;; of the same name in order from most to least specific.
         ;; The different slotds are sorted least specific first.
         (direct-slots
           (loop with result = nil
                 for class in cpl
                 do (loop for slotd in (reverse (direct-slots class))
                          for name = (name slotd)
                          for existing = (assoc name result)
                          when existing
                            do (push slotd (cdr existing))
                          else
                            do (push (list name slotd) result))
                 finally (return result))))
    (loop for (_ . direct-slotds) in direct-slots
          for i from 0
          collect (compute-effective-slot direct-slotds i))))

(defun compute-default-initargs (cpl)
  ;; We didn't do the canonicalization stuff out of laziness and
  ;; because initfunctions don't make much sense here, so this is
  ;; just append with duplicates removed.
  (loop with seen = ()
        for class in cpl
        for inits = (direct-default-initargs class)
        nconc (loop for (k v) on inits by #'cddr
                    unless (member k seen)
                      do (push k seen)
                      and collect k
                      and collect v)))

(defun finalizedp (class)
  (slot-boundp class '%class-precedence-list))

(defun finalize-inheritance (class)
  (let* ((supers (direct-superclasses class))
         (_ (loop for sup in supers
                  unless (finalizedp sup)
                    do (finalize-inheritance sup)))
         (cpl (compute-class-precedence-list class supers))
         (effective-slotds (compute-slots cpl))
         (default-initargs (compute-default-initargs cpl)))
    (declare (ignore _))
    (setf (class-precedence-list class) cpl
          (slots class) effective-slotds
          (default-initargs class) default-initargs))
  (values))

(defun initialize-compiler-class (class name supers slotds options
                                  &key (find-class
                                        #'cross-clasp:find-compiler-class))
  (multiple-value-bind (metaclass documentation default-initargs)
      (parse-class-options options)
    (declare (ignore documentation)) ; FIXME?
    (let* ((metaclass (or metaclass 'standard-class))
           (supers (or supers
                     (ecase metaclass
                       ((standard-class) '(standard-object))
                       ((funcallable-standard-class)
                        '(funcallable-standard-object))
                       ((built-in-class)
                        (if (eq name t) ; weird special case
                            ()
                            '(t))))))
           (supers (mapcar find-class supers))
           (slotds (mapcar #'make-compiler-slotd slotds))
           (rmetaclass (funcall find-class metaclass)))
      (reinitialize-instance
       class
       :supers supers :direct-slots slotds
       :direct-default-initargs default-initargs
       :metaclass rmetaclass)
      (finalize-inheritance class)
      (loop for super in supers
            do (push class (direct-subclasses super)))
      class)))

(defun make-compiler-class (name supers slotds options)
  (initialize-compiler-class (make-instance 'compiler-class
                               :name name)
                             name supers slotds options))

(defun primitive-accessor (class)
  (ecase (name (metaclass class))
    (standard-class 'standard-instance-access)
    (funcallable-standard-class 'funcallable-standard-instance-access)))

(defmacro early-initialize-instance (class-name object &rest initargs)
  (let* ((class (cross-clasp:find-compiler-class class-name))
         ;; FIXME: This won't actually work since it can trip the
         ;; duplicate initargs thing below, but I don't care right now.
         (initargs (append initargs (default-initargs class)))
         (sia (primitive-accessor class))
         (slots (slots class))
         (o (gensym "OBJECT")))
    `(let ((,o ,object))
       (setf
        ,@(loop with invalid-keys
                for (key val) on initargs by #'cddr
                for slotd = (loop for slotd in slots
                                  when (member key (initargs slotd))
                                    return slotd
                                  finally (push key invalid-keys)
                                          (return nil))
                when slotd
                  collect `(,sia ,o ,(location slotd))
                  and collect val
                  and do (setf slots (remove slotd slots))
                finally (unless (null invalid-keys)
                          (error "Unrecognized or duplicate initargs: ~s"
                                 invalid-keys)))
        ;; Initialize other slots with initforms, if they have em.
        ,@(loop for slotd in slots
                when (initformp slotd)
                  collect `(,sia ,o ,(location slotd))
                  and collect (initform slotd)))
       ,o)))

(defmacro early-make-instance (class-name &rest initargs)
  `(early-initialize-instance
    ,class-name
    (core:allocate-standard-instance
     (find-class ',class-name)
     ,(length (slots (cross-clasp:find-compiler-class class-name))))
    ,@initargs))

;; returns a bunch of bindings for macrolet.
(defun early-accessors (class)
  (loop with sia = (primitive-accessor class)
        for slotd in (slots class)
        for loc = (location slotd)
        ;; for accessors we just use all possible readers, even if
        ;; there's no corresponding writers.
        ;; this is early! chaos reigns!
        for accessors = (readers slotd)
        nconc (loop for acc in accessors
                    collect `(,acc (object)
                               (list ',sia object ',loc)))))

(defmacro with-early-accessors ((&rest class-names) &body body)
  `(macrolet (,@(loop for name in class-names
                      for class = (cross-clasp:find-compiler-class name)
                      nconc (early-accessors class)))
     ,@body))

(defun build-direct-slot-form (compiler-slotd)
  `(early-make-instance standard-direct-slot-definition
                        :name ',(name compiler-slotd)
                        :initform ,(if (initformp compiler-slotd)
                                       `',(initform compiler-slotd)
                                       '+initform-unsupplied+)
                        :initfunction ,(if (initformp compiler-slotd)
                                           `#'(lambda ()
                                                ,(initform compiler-slotd))
                                           nil)
                        :initargs ',(initargs compiler-slotd)
                        :readers ',(readers compiler-slotd)
                        :writers ',(writers compiler-slotd)
                        :type ',(stype compiler-slotd)
                        :location ',(location compiler-slotd)))

(defun build-slot-form (compiler-slotd)
  `(early-make-instance standard-effective-slot-definition
                        :name ',(name compiler-slotd)
                        :initform ,(if (initformp compiler-slotd)
                                       `',(initform compiler-slotd)
                                       '+initform-unsupplied+)
                        :initfunction ,(if (initformp compiler-slotd)
                                           `#'(lambda ()
                                                ,(initform compiler-slotd))
                                           nil)
                        :initargs ',(initargs compiler-slotd)
                        :readers ',(readers compiler-slotd)
                        :writers ',(writers compiler-slotd)
                        :type ',(stype compiler-slotd)
                        :location ',(location compiler-slotd)))

(defun canonicalized-default-initargs-form (default-initargs)
  `(list
    ,@(loop for (k v) on default-initargs by #'cddr
            collect `(list ',k ',v #'(lambda () ,v)))))

(defun initialize-class-form (var class)
  `(progn
     ;; EARLY-INITIALIZE-INSTANCE takes care of initforms
     ;; and such. This also reduces special casing, hopefully.
     (early-initialize-instance
      ,(name (metaclass class)) ,var
      :name ',(name class)
      :direct-superclasses (list ,@(loop for super
                                           in (direct-superclasses class)
                                         for sname = (name super)
                                         collect `(find-class ',sname)))
      :direct-slots (list ,@(loop for slot in (direct-slots class)
                                  collect (build-direct-slot-form
                                           slot)))
      ;; since default-initargs is set separately there can be
      ;; duplicate initfunctions, but I do not care.
      :direct-default-initargs ,(canonicalized-default-initargs-form
                                 (direct-default-initargs class)))
     (with-early-accessors (std-class)
       (setf (%class-slots ,var)
             (list ,@(loop for slot in (slots class)
                           collect (build-slot-form slot)))
             (%class-precedence-list ,var)
             (list ,@(loop for s in (class-precedence-list class)
                           collect `(find-class ',(name s))))
             (%class-default-initargs ,var)
             ,(canonicalized-default-initargs-form
               (default-initargs class))
             (%class-direct-subclasses ,var)
             (list ,@(loop for sub
                             in (direct-subclasses class)
                           for sname = (name sub)
                           collect `(find-class ',sname)))
             (%class-finalized-p ,var) t
             (class-size ,var) ,(length (slots class))))
     ,var))

(defmacro early-defclass (name (&rest supers) (&rest slotds) &rest options)
  (let ((class (make-compiler-class name supers slotds options)))
    `(progn
       (eval-when (:compile-toplevel)
         (setf (find-class ',name) ,class))
       (eval-when (:load-toplevel :execute)
         (let ((class
                  (or (find-class ',name nil)
                    (let* ((metaclass (find-class ',(name (metaclass class))))
                           (slotds (with-early-accessors (std-class)
                                     (class-slots metaclass)))
                           (size (length slotds))
                           (stamp (core:class-stamp-for-instances metaclass)))
                      (core:allocate-raw-instance
                       metaclass
                       (core:make-rack
                        size slotds stamp (core:unbound)))))))
           ;; Install class.
           ;; we do this first so the CPL can refer to the class.
           (core:setf-find-class class ',name)
           ;; Initialize rack slots.
           ,(initialize-class-form 'class class))))))

;;; Welcome to the deep magic.
;;; This macro allows defmacro forms as its toplevel to refer to each
;;; other kind of like letrec. For example it's okay to have classes
;;; with themselves as a metaclass, or with a metaclass that's defined
;;; later, bla bla bla. This is all needed to describe CLOS.
;;; The basic goal here is for all the base class definitions to be
;;; meaningfully described by simple and clear DEFCLASS forms.
;;; This includes being able to _change_ these definitions in the future,
;;; so the magic does _not_ include that kind of special casing.
;;; Here's what we do:
;;; 1) at compile time: make all compiler classes, then fill them in
;;; 2) at load time: first, make all classes
;;; 3) fill them in
;;; 4) compute sigs so all the class instances are not obsolete.
(defmacro with-mutual-defclass (&body body)
  (loop for form in body
        unless (and (consp form) (eq (car form) 'defclass))
          do (error "Only DEFCLASS forms are allowed here."))
  ;; Before any expansion we set up compiler classes.
  ;; These are dumped as literals, but only into :compile-toplevel code
  ;; so they don't end up in the FASL.
  ;; Note that we refer back to this list rather than doing
  ;; (setf find-class) at macroexpansion time, so that expanding this
  ;; macro does not have or rely on side effects.
  (let ((compiler-classes
          (loop for (_ name) in body
                for class = (make-instance 'compiler-class :name name)
                collect (cons name class))))
    ;; initialize the classes
    (loop for (_1 _2 supers slotds . options) in body
          for (name . cclass) in compiler-classes
          do (initialize-compiler-class
              cclass name supers slotds options
              :find-class (lambda (name)
                            (or (cdr (assoc name compiler-classes))
                              (error "No class: ~s" name)))))
    ;; and finalize their inheritance.
    (loop for (_ . cclass) in compiler-classes
          unless (finalizedp cclass) do (finalize-inheritance cclass))
    ;; All compiler classes are done, let's expand.
    `(progn
       (eval-when (:compile-toplevel)
         ,@(loop for (name . class) in compiler-classes
                 collect `(setf (find-class ',name) ',class)))
       (eval-when (:load-toplevel :execute)
         ;; Here is what the FASL is actually going to do.
         ;; First, make all the classes. We already have all class sizes,
         ;; so this isn't much of a problem.
         ;; Note that the primordial image already has several classes
         ;; defined, such as STANDARD-CLASS. This makes our job here easier.
         ;; With fewer primordial classes, we'd need to do some toposorting
         ;; or something, and special case standard-class.
         ,@(loop for (name . class) in compiler-classes
                 for metaclass = (metaclass class)
                 collect `(unless (find-class ',name nil)
                            (core:class-new-stamp
                             (core:setf-find-class
                              (core:allocate-standard-instance
                               (find-class ',(name metaclass))
                               ,(length (slots metaclass)))
                              ',name)
                             ',name)))
         ;; Now we fill in all the classes. We can use the compile-time
         ;; CPLs and effective slots. The standard-direct-slot-definition
         ;; etc classes have already been made so we can make instances of
         ;; them without an issue.
         ,@(loop for (name . class) in compiler-classes
                 for metaclass = (metaclass class)
                 collect `(let ((class (find-class ',name)))
                            ,(initialize-class-form 'class class)))
         ;; Finally, go through the classes setting the sigs of
         ;; their slotds, which did not exist when they were created.
         (with-early-accessors (std-class)
           ,@(loop for (name . class) in compiler-classes
                   collect `(let ((class (find-class ',name)))
                              (loop for s in (class-slots class)
                                    do (core:instance-sig-set s))
                              (loop for s in (class-direct-slots class)
                                    do (core:instance-sig-set s)))))))))

;;; Define, in the compiler, some basic classes that we need to
;;; compile much of anything in CLOS.
#+(or)
(defun install-basic-hierarchy ()
  ;; metaclass is unset because those classes don't yet exist.
  (setf (cross-clasp:find-compiler-class 't)
        (make-instance 'compiler-class
          :name 't :supers () :direct-slots ()))
  (finalize-inheritance (cross-clasp:find-compiler-class 't)))
