(in-package #:cross-clasp.clasp.clos)

(defmethod anatomicl:find-class ((client cross-clasp:client) symbol
                                 &optional (errorp t) environment)
  (declare (ignore errorp environment))
  (cross-clasp:find-compiler-class symbol))

(defmethod anatomicl:structure-class-name ((client cross-clasp:client)) 'structure-class)
(defmethod anatomicl:structure-object-name ((client cross-clasp:client))
  'structure-object)

;; We want to override anatomicl's usual method, which expands into an anatomicl function.
;; We also skip the CHECK-TYPE since the expansion's nontrivial I guess, FIXME
(defmethod anatomicl::generate-copier ((client cross-clasp:client)
                                       (desc anatomicl::defstruct-object-description)
                                       layout copier-name)
  (declare (ignore layout))
  `(defun ,copier-name (object) (copy-structure object)))

(defmethod anatomicl::generate-setf-structure-description ((client cross-clasp:client)
                                                           desc)
  (declare (ignore desc))
  (error "~s not implemented yet TODO" 'anatomicl::generate-setf-structure-description))

(defmethod anatomicl::generate-slot-initialization-form ((client cross-clasp:client)
                                                         (desc anatomicl::defstruct-object-description)
                                                         layout obj slot value)
  ;; TODO? Could be more efficient by grabbing the rack beforehand?
  `(setf (standard-instance-access ,obj ,(position slot layout)) ,value))

(defun parse->slotd (anaslotd)
  ;; accessors are handled elsewhere
  `(,(anatomicl::slot-name anaslotd)
    ,@(when (anatomicl::slot-initform-p anaslotd)
        `(:initform ,(anatomicl::slot-initform anaslotd)))
    ,(if (anatomicl::slot-read-only anaslotd) ':reader ':accessor)
    ,(anatomicl::slot-accessor-name anaslotd)
    :type ,(anatomicl::slot-type anaslotd)))

(defmethod anatomicl::generate-defstruct-bits ((client cross-clasp:client)
                                               (desc anatomicl::defstruct-object-description)
                                               layout environment)
  (declare (ignore environment))
  `(progn
     (defclass ,(anatomicl::defstruct-name desc)
         (,(or (anatomicl::defstruct-included-structure-name desc)
             'structure-object))
       (,@(mapcar #'parse->slotd layout))
       (:metaclass structure-class))
     ,@(when (anatomicl::defstruct-print-object desc)
         (list `(defmethod print-object ((object ,(anatomicl::defstruct-name desc))
                                         stream)
                  (funcall (function ,(anatomicl::defstruct-print-object desc))
                           object stream))))))

(defmacro early-defstruct (name-and-options &rest slots &environment env)
  (anatomicl:expand-defstruct maclina.machine:*client*
                              (anatomicl:parse-defstruct name-and-options slots)
                              env))
