(in-package #:cross-clasp.clasp.clos)

(defmacro early-defstruct (name-and-options &rest slot-descriptions)
  (expand-defstruct
   (anatomicl:parse-defstruct name-and-options slot-descriptions)))

(defgeneric expand-defstruct (description))

(defun keywordify (string-designator)
  (intern (string string-designator) "KEYWORD"))

(defun defstruct-slot->defclass-slot (slot)
  (nconc (list (anatomicl::slot-name slot)
               :initarg (keywordify (anatomicl::slot-name slot)))
         ;; clhs defstruct says initforms are like providing initforms to
         ;; keyword parameters of constructors, which implies that they are
         ;; evaluated in the lexical environment of the structure, even if they
         ;; are inherited. That's real complicated and I don't care about it here.
         (if (anatomicl::slot-initform-p slot)
             (list :initform (anatomicl::slot-initform slot))
             nil)
         (list :reader (anatomicl::slot-accessor-name slot))
         (if (anatomicl::slot-read-only slot)
             nil
             (list :writer `(setf ,(anatomicl::slot-accessor-name slot))))
         (list :type (anatomicl::slot-type slot))))

(defun generate-standard-constructor (class name)
  (let* ((slots (slots class))
         (suppliedp-syms (loop for slot in slots
                               for name = (concatenate 'string
                                                       (string (name slot)) "-P")
                               collect (make-symbol name)))
         (slot-name-syms (loop for slot in slots
                               collect (make-symbol (string (name slot))))))
    `(defun ,name (&key ,@(loop for slot in slots
                                for name in slot-name-syms
                                for suppliedp in suppliedp-syms
                                collect `(,name ,(initform slot) ,suppliedp)))
       (let ((object (early-allocate-instance ,(name class))))
         ,@(loop for slot in slots
                 for loc = (location slot)
                 for name in slot-name-syms
                 for suppliedp in suppliedp-syms
                 collect `(when ,suppliedp
                            (setf (standard-instance-access object ,loc) ,name)))
         object))))

(defun generate-constructors (description class)
  (loop for constructor in (anatomicl::defstruct-constructors description)
        collect (if (cdr constructor)
                    (error "BOA constructors not yet supported")
                    (generate-standard-constructor class (first constructor)))))

(defun generate-predicate (class name)
  `(defun ,name (object)
     (core::subclassp (class-of object) (find-class ',(name class)))))

(defun generate-predicates (description class)
  (loop for predicate-name in (anatomicl::defstruct-predicates description)
        collect (generate-predicate class predicate-name)))

(defun generate-copier (class name)
  (declare (ignore class))
  `(defun ,name (object)
     (copy-structure object)))

(defun generate-copiers (description class)
  (loop for copier-name in (anatomicl::defstruct-copiers description)
        collect (generate-copier class copier-name)))

(defmethod expand-defstruct ((description anatomicl::defstruct-object-description))
  (let* ((name (anatomicl::defstruct-name description))
         (include (anatomicl::defstruct-included-structure-name description))
         (supers (if include (list include) '(structure-object)))
         (slotds (append
                  (mapcar #'defstruct-slot->defclass-slot
                          (anatomicl::defstruct-included-slots description))
                  (mapcar #'defstruct-slot->defclass-slot
                          (anatomicl::defstruct-direct-slots description))))
         (options '((:metaclass structure-class)))
         (class (make-compiler-class name supers slotds options)))
    `(progn
       ,(expand-early-defclass class)
       ,@(when (anatomicl::defstruct-print-object description)
           (list `(defmethod print-object
                      ((object ,(anatomicl::defstruct-name description)) stream)
                    (funcall #',(anatomicl::defstruct-print-object description)
                             object stream))))
       ,@(generate-constructors description class)
       ,@(generate-predicates description class)
       ,@(generate-copiers description class)
       ',name)))
