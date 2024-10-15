(in-package #:clos)

(defconstant +initform-unsupplied+ '+initform-unsupplied+)

(defun standard-instance-access (instance location)
  (core:rack-ref (core:instance-rack instance) location))
(defun (setf standard-instance-access) (new instance location)
  (setf (core:rack-ref (core:instance-rack instance) location) new))
(defun funcallable-standard-instance-access (instance location)
  (core:rack-ref (core:instance-rack instance) location))
(defun (setf funcallable-standard-instance-access)
    (new instance location)
  (setf (core:rack-ref (core:instance-rack instance) location) new))

(with-mutual-defclass

(defclass t ()
  ()
  (:metaclass built-in-class))

(defclass standard-object (t) ())

(defclass metaobject () ())

(defclass specializer (metaobject)
  ;; The number of specializer slots is fixed in instance.h.
  ;; A change in the number of slots here needs to be reflected there.
  ;; The slots marked with a :location are also fixed in instance.h.
  ;; They need to have those locations, even in user subclasses of this class.
  ;; Also note that boot.lisp ignores these locations for effective slots, just
  ;; using the position in the list here; so that must match the :location.
  ;; It checks this.
  ;; Any changes to the slots below need to be reflected in instance.h
  ((direct-methods :initform nil :reader specializer-direct-methods
                   :accessor %specializer-direct-methods)
   (call-history-generic-functions
    :initform nil
    :reader specializer-call-history-generic-functions
    :location 1)
   (specializer-mutex :initform (mp:make-shared-mutex
                                 'call-history-generic-functions-mutex)
                      :reader specializer-mutex :location 2)
   ;; Any changes to the slots above need to be reflected in instance.h
   ))

(defclass class (specializer) ())

(defclass std-class (class)
  ((name :initarg :name :initform nil :reader class-name :location 3)
   (direct-superclasses :initarg :direct-superclasses :initform nil
			:reader class-direct-superclasses :location 4
                        :accessor %class-direct-superclasses)
   (direct-subclasses :initform nil  :location 5
                      :reader class-direct-subclasses
                      :accessor %class-direct-subclasses)
   (slots :reader class-slots :accessor %class-slots :location 6)
   (precedence-list :reader class-precedence-list
                    :accessor %class-precedence-list :location 7)
   (direct-slots :initarg :direct-slots :reader class-direct-slots :location 8
                 :initform nil :accessor %class-direct-slots)
   (direct-default-initargs :initarg :direct-default-initargs :location 9
			    :initform nil :reader class-direct-default-initargs)
   (default-initargs :reader class-default-initargs
                     :accessor %class-default-initargs :location 10)
   (finalized :initform nil :reader class-finalized-p
              :accessor %class-finalized-p :location 11)
   (docstring :initarg :documentation :initform nil :location 12)
   (size :accessor class-size)
   (prototype)
   (dependents :initform nil :accessor class-dependents :location 15)
   (valid-initargs :accessor class-valid-initargs)
   (location-table :initform nil :accessor class-location-table :location 17)
   (stamp-for-instances :accessor stamp-for-instances :location 18)
   (creator :accessor creator :location 19)
   (source-position :initform nil :initarg :source-position :accessor class-source-position)))

(defclass standard-class (std-class) ())
(defclass built-in-class (std-class) ()) ; see "cut down" below
  
(defclass slot-definition (metaobject) ())
(defclass direct-slot-definition (slot-definition)
  ((%effective-readers :initform nil :reader %effective-readers)
   (%effective-writers :initform nil :reader %effective-writers)))
(defclass effective-slot-definition (slot-definition) ())
(defclass standard-slot-definition (slot-definition)
  ((name :initarg :name :initform nil :reader slot-definition-name)
   (initform :initarg :initform :initform +initform-unsupplied+
             :reader slot-definition-initform)
   (initfunction :initarg :initfunction :initform nil
                 :reader slot-definition-initfunction)
   (declared-type :initarg :type :initform t :reader slot-definition-type)
   (allocation :initarg :allocation :initform :instance :reader slot-definition-allocation)
   (initargs :initarg :initargs :initform nil :reader slot-definition-initargs)
   (readers :initarg :readers :initform nil :reader slot-definition-readers)
   (writers :initarg :writers :initform nil :reader slot-definition-writers)
   (docstring :initarg :documentation :initform nil :accessor slot-definition-documentation)
   ;; in here because clasp sometimes allows it to be specified in
   ;; direct slots, as an extension.
   (location :initarg :location :initform nil :reader slot-definition-location
             :accessor %slot-definition-location)))
(defclass standard-effective-slot-definition (standard-slot-definition) ())
(defclass standard-direct-slot-definition (standard-slot-definition
                                           direct-slot-definition)
  (;; see effective-accessor.lisp
   (%effective-readers :initform nil :reader %direct-slotd-effective-readers)
   (%effective-writers :initform nil :reader %direct-slotd-effective-writers)))

(defclass funcallable-standard-class (std-class) ())

(defclass function () () (:metaclass built-in-class))

(defclass funcallable-standard-object (function standard-object)
  ()
  (:metaclass funcallable-standard-class))

(defclass generic-function (metaobject funcallable-standard-object)
  ()
  (:metaclass funcallable-standard-class))

(defclass standard-generic-function (generic-function)
  (;; A description of how the methods on this generic function are
   ;; specialized. It's a simple-vector with as many elements as the gf
   ;; has required arguments. If a parameter is unspecialized (i.e.
   ;; all methods' specializers there are T), that element is NIL.
   ;; If one or more methods have an eql specializer at that position,
   ;; the element is a list of their eql specializer objects.
   ;; Otherwise (i.e. the parameter is specialized with non eql
   ;; specializers) the element is T.
   (specializer-profile :initform nil
                        :accessor generic-function-specializer-profile)
   ;; An alist of (specializer-key . outcome) representing previously
   ;; seen calls to this function. A specializer-key is a vector of
   ;; the direct specializers of the required arguments in the call,
   ;; and an outcome is as in outcome.lisp.
   (call-history :initform nil :accessor generic-function-call-history)
   (method-combination
    :initarg :method-combination
    :initform (find-method-combination (class-prototype (find-class 'standard-generic-function))
                                       'standard nil)
    :reader generic-function-method-combination
    :accessor %generic-function-method-combination)
   ;; NOTE about generic function lambda lists.
   ;; AMOP says rather specifically that the original lambda list
   ;; passed to ensure-generic-function can be read, and that the
   ;; implementation can't alter it. That's this.
   ;; But we use the underlying function lambda list as well, for
   ;; display. That's what maybe-augment in method.lisp deals with,
   ;; and what ext:function-lambda-list returns.
   (lambda-list :initarg :lambda-list
                :reader generic-function-lambda-list)
   (argument-precedence-order 
    :initarg :argument-precedence-order
    :initform nil
    :reader generic-function-argument-precedence-order
    :accessor %generic-function-argument-precedence-order)
   (method-class
    :initarg :method-class
    :initform (find-class 'standard-method)
    :reader generic-function-method-class)
   (methods :initform nil :reader generic-function-methods
            :accessor %generic-function-methods)
   (a-p-o-function :initform nil :accessor generic-function-a-p-o-function)
   (declarations
    :initarg :declarations
    :initform nil
    :reader generic-function-declarations)
   (dependents :initform nil :accessor generic-function-dependents)
   ;; An indicator that the GF is being traced somehow.
   ;; If not being traced, this is NIL (the default).
   ;; Otherwise, it's a cons. The car of the cons is either
   ;; :PROFILE-ONGOING, meaning dispatch misses are printed to
   ;; *TRACE-OUTPUT*, or
   ;; :PROFILE-RECORD, meaning they aren't. In either case, the
   ;; cadr is then the overhead recorded in seconds, and the
   ;; cddr is a list of argument lists that have caused misses.
   ;; More to come. See telemetry.lisp for interface.
   (tracy :initform nil :accessor %generic-function-tracy
          :type list))
  (:metaclass funcallable-standard-class))

(defclass method (metaobject) ())
(defclass std-method (method)
  ((the-function :initarg :function :reader method-function)))
(defclass standard-method (std-method)
  ((the-generic-function :initarg :generic-function :initform nil
                         :reader method-generic-function
                         ;; Writer rather than accessor for the somewhat KLUDGEy
                         ;; reason that satiate-readers (in satiate.lisp) would try to
                         ;; satiate it for effective-*-method otherwise, and they don't
                         ;; have a method on it.
                         :writer (setf %method-generic-function))
   (lambda-list :initarg :lambda-list
                :reader method-lambda-list)
   (specializers :initarg :specializers :reader method-specializers)
   (qualifiers :initform nil :initarg :qualifiers :reader method-qualifiers)
   (docstring :initarg :documentation :initform nil)
   ;; Usually we just use the function's source position, but
   ;; sometimes this is inadequate, e.g. for accessors, which share
   ;; a method-function.
   ;; So for those we use this - but not normal DEFMETHOD.
   (source-position :initform nil :initarg :source-position
                    :accessor method-source-position)
   (plist :initform nil :initarg :plist :accessor method-plist)
   ;; these are the precomputed results of cl:function-keywords.
   (keywords :initform nil :initarg :keywords :accessor method-keywords)
   (aok-p :initform nil :initarg :aok-p :accessor method-allows-other-keys-p)
   ;; leaf-method-p is T if the method form doesn't call call-next-method or next-method-p
   ;; our custom initargs are internal symbols, as per MOP "The defmethod macros"
   (leaf-method-p :initform nil :initarg leaf-method-p :reader leaf-method-p)))

(defclass standard-accessor-method (standard-method)
  ((slot-definition :initarg :slot-definition :initform nil
                    :reader accessor-method-slot-definition)))

(defclass standard-reader-method (standard-accessor-method) ())
(defclass standard-writer-method (standard-accessor-method) ())

;; needed to make accessor method functions
(defclass %leaf-method-function (funcallable-standard-object)
  ((%fmf :initarg :fmf :reader fmf))
  (:metaclass funcallable-standard-class))
) ; with-mutual-defclass

;;; Enough classes now exist that we can use the "real" but still
;;; early defclass macro (does not invoke generics, etc.).

;;; Used in discriminating function computation.
(defclass effective-accessor-method (method)
  ((%original :initarg :original :reader effective-accessor-method-original)
   (%location :initarg :location :reader effective-accessor-method-location)))
(defclass effective-reader-method (effective-accessor-method) ())
(defclass effective-writer-method (effective-accessor-method) ())

;;; These should really be cut down - I mean, since when do they have
;;; a need for finalizedp and default initargs?
(defclass forward-referenced-class (std-class) ())
(defclass core:cxx-class (std-class) ())
(defclass core:clbind-cxx-class (std-class) ())
(defclass core:derivable-cxx-class (std-class) ())

(defclass eql-specializer (specializer)
  ((object :initarg :object :reader eql-specializer-object)))

;;; maybe also needs trimming?
(defclass structure-class (std-class)
  ;; Note that we don't need some of the class-slots, e.g. initargs, so we
  ;; could hypothetically reorganize things.
  ;; We also don't really need any of these slots, but it might be good to have
  ;; some kind of structure to represent descriptions of structures later.
  (slot-descriptions
   initial-offset
   constructors))

(defclass structure-object (t)
  ()
  (:metaclass structure-class))
