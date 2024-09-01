(in-package #:cross-clasp.clasp.core)

(defmacro with-unique-names (symbols &body body)
  `(let* ,(mapcar (lambda (symbol)
                    (let* ((symbol-name (symbol-name symbol))
                           (stem symbol-name))
                      `(,symbol (gensym ,stem))))
                  symbols)
     ,@body))

(defmacro once-only (specs &body body)
  "Once-Only ({(Var Value-Expression)}*) Form*

Create a Let* which evaluates each Value-Expression, binding a
temporary variable to the result, and wrapping the Let* around the
result of the evaluation of Body.  Within the body, each Var is bound
to the corresponding temporary variable.

Bare symbols in `specs' are equivalent to:

  (symbol symbol)

Example:

  (defmacro cons1 (x)
    (once-only (x) `(cons ,x ,x)))
  (let ((y 0))
    (cons1 (incf y)))
  ; => (1 . 1)
"
  (labels ((frob (specs body)
             (if (null specs)
                 `(progn ,@body)
                 (let ((spec (first specs)))
                   (cond ((atom spec)
                          (setf spec (list spec spec)))
                         ((/= (length spec) 2)
                          (error "Malformed Once-Only binding spec: ~S." spec)))
                   (let ((name (first spec))
                         (exp-temp (gensym)))
                     `(let ((,exp-temp ,(second spec))
                            (,name (gensym "OO-")))
                        (list 'let (list (list ,name ,exp-temp))
                              ,(frob (rest specs) body))
                        #+(or) ; can't use host quasiquote
                        `(let ((,,name ,,exp-temp))
                           ,,(frob (rest specs) body))))))))
    (frob specs body)))

(defmacro defconstant-eqx (name form test &rest rest)
  `(defconstant ,name
     (let ((value ,form))
       (cond ((not (boundp ',name)) value)
             ((,test (symbol-value ',name) value)
              (symbol-value ',name))
             (t value))) ; probably error
     ,@rest))

(defmacro defconstant-equal (name form &rest rest)
  `(defconstant-eqx ,name ,form equal ,@rest))

(defun process-declarations (body &optional docstringp)
  (multiple-value-bind (body decls doc)
      (alexandria:parse-body body :documentation docstringp)
    (let* ((decls (mapcan (lambda (L) (copy-list (cdr L))) decls))
           (specials
             (loop for thing in decls
                   when (and (consp thing)
                             (eq (car thing) 'special))
                     append (cdr thing))))
      (values decls body doc specials))))

(defun process-lambda-list (lambda-list context)
  (declare (ignore context)) ; FIXME?
  (multiple-value-bind (req opt rest keywords aokp aux keyp)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (values (cons (length req) req)
            (cons (length opt) (apply #'append opt))
            rest
            keyp
            (cons (length keywords)
                  (loop for ((key var) default -p) in keywords
                        collect key collect var collect default collect -p))
            aokp
            (apply #'append aux)
            ;; varest i think, fixme?
            nil)))

(defun search-keyword (list key)
  (cond ((atom list) 'missing-keyword)
	((atom (cdr list)) 'missing-keyword)
	((eq (car list) key) (cadr list))
	(t (search-keyword (cddr list) key))))

(defun simple-program-error (datum &rest args) (apply #'error datum args))

(defun check-keywords (tail keywords &optional allow-other-keys)
  (do (head arg
            ;; has :allow-other-keys been found? only the first counts
            aok-flag
       (err nil))
      ((null tail)
       (when (and err (not allow-other-keys))
	 (simple-program-error "The keys ~s are not allowed" err)))
    (if (atom tail)
      (simple-program-error "keyword list is not a proper list")
      (setq head (car tail) tail (cdr tail)))
    (if (atom tail)
      (simple-program-error "keyword list is not a proper list")
      (setq arg (car tail) tail (cdr tail)))
    (cond ((eq head :allow-other-keys)
           ;; :allow-other-keys nil doesn't override &allow-other-keys.
           (unless (or allow-other-keys aok-flag)
             (setq allow-other-keys arg))
           (setq aok-flag t))
          ((not (member head keywords))
           (push head err)))))

(defun check-compiler-macro-keywords
    (tail keywords &optional allow-other-keys)
  (do (head
       arg
       aok-flag
       ;; List of keywords not allowed by the lambda list.
       (err nil)
       ;; List of keywords that are variable forms, like in
       ;; (defun foo (&key ...) ...) (foo (if a :x :y) ...)
       (variable nil))
      ((null tail)
       (when (and err (not allow-other-keys))
	 (simple-program-error "The keys ~s are not allowed" err))
       variable)
    (if (atom tail)
      (simple-program-error "keyword list is not a proper list")
      (setq head (car tail) tail (cdr tail)))
    (if (atom tail)
      (simple-program-error "keyword list is not a proper list")
      (setq arg (car tail) tail (cdr tail)))
    (cond ((eq head :allow-other-keys)
           (unless (or allow-other-keys aok-flag)
             (setq allow-other-keys arg))
           (setq aok-flag t))
          ;; FIXME: Constants should also be okay, but we don't have
          ;; ext:constant-form-value yet.
          ((not (keywordp head))
           (push head variable))
          ((not (member head keywords))
           (push head err)))))

(defun dm-too-many-arguments (current-form vl macro-name)
  (error 'destructure-wrong-number-of-arguments
         :macro-name macro-name :lambda-list vl :arguments current-form
         :problem :too-many))

(defun dm-too-few-arguments (current-form vl macro-name)
  (error 'destructure-wrong-number-of-arguments
         :macro-name macro-name :lambda-list vl :arguments current-form
         :problem :too-few))

(defun destructure (vldestructure context
                    &optional display-name cm-name
                    &aux dl arg-check (basis-form (gensym))
                      (destructure-symbols (list basis-form)))
  (labels ((tempsym ()
	     (let ((x (gensym)))
	       (push x destructure-symbols)
	       x))
           (whole-in-context (whole context)
             (if (eq context 'destructuring-bind)
                 whole
                 ;; Special handling if define-compiler-macro called this
                 (if (eq context 'cl:define-compiler-macro)
                     `(if (and (eq (car ,whole) 'cl:funcall)
                               (consp (cadr ,whole))
                               (eq (caadr ,whole) 'cl:function)
                               (consp (cdadr ,whole))
                               (equal (second (second ,whole)) ',cm-name))
                          (cddr (the cons ,whole))
                          (cdr (the cons ,whole)))
                     ;; deftype- allow bare symbols- symbol = (symbol)
                     (if (eq context 'cl:deftype)
                         `(if (symbolp ,whole)
                              nil
                              (cdr (the cons ,whole)))
                         ;; defmacro (or setf expander)
                         `(cdr (the cons ,whole))))))
	   (dm-vl (vldestructure whole context)
	     (multiple-value-bind (reqs opts rest key-flag keys allow-other-keys auxs)
		 (process-lambda-list vldestructure context)
	       (let* ((pointer (tempsym))
		      (cons-pointer `(the cons ,pointer))
		      (unsafe-car `(car ,cons-pointer))
		      (unsafe-cdr `(cdr ,cons-pointer))
		      (unsafe-pop `(setq ,pointer ,unsafe-cdr))
		      (no-check nil)
		      all-keywords)
		 ;; In macros, eliminate the name of the macro from the list
		 (dm-v pointer (whole-in-context whole context))
		 (dolist (v (cdr reqs))
		   (dm-v v `(progn
			      (if (null ,pointer)
				  (dm-too-few-arguments
                                   ,basis-form ',vldestructure ',display-name))
			      (prog1 ,unsafe-car ,unsafe-pop))))
		 (dotimes (i (pop opts))
		   (let* ((x (first opts))
			  (init (second opts))
			  (sv (third opts)))
		     (setq opts (cdddr opts))
		     (cond (sv
			    (dm-v x `(if ,pointer ,unsafe-car ,init))
			    (dm-v sv `(and ,pointer (progn ,unsafe-pop t))))
			   (t
			    (dm-v x `(if ,pointer
					 (prog1 ,unsafe-car ,unsafe-pop)
					 ,init))))))
		 (when rest
		   (dm-v rest pointer)
		   (setq no-check t))
		 (dotimes (i (pop keys))
		   (let* ((temp (tempsym))
			  (k (first keys))
			  (v (second keys))
			  (init (third keys))
			  (sv (fourth keys)))
		     (setq no-check t)
		     (setq keys (cddddr keys))
		     (dm-v temp `(search-keyword ,pointer ',k))
		     (dm-v v `(if (eq ,temp 'missing-keyword) ,init ,temp))
		     (when sv (dm-v sv `(not (eq ,temp 'missing-keyword))))
		     (push k all-keywords)))
		 (do ((l auxs (cddr l))) ((endp l))
		   (let* ((v (first l))
			  (init (second l)))
		     (dm-v v init)))
		 (cond (key-flag
			(push
                         (if (eq context 'define-compiler-macro)
                             `(check-compiler-macro-keywords ,pointer ',all-keywords
                                                            ,(if allow-other-keys t nil))
                             `(check-keywords ,pointer ',all-keywords
                                              ,(if allow-other-keys 't 'nil)))
                         arg-check))
		       ((not no-check)
			(push `(if ,pointer
                                   (dm-too-many-arguments
                                    ,basis-form ',vldestructure ',display-name))
			      arg-check))))))
	   (dm-v (v init)
	     (cond ((and v (symbolp v))
                    (let ((push-val (if init (list v init) v)))
                      (push push-val dl)))
                   ((and v (atom v))
                    (error "destructure: ~A is not a list nor a symbol" v))
                   ((eq (first v) '&whole)
                    (let ((whole-var (second v)))
                      (if (listp whole-var)
                          (let ((new-whole (tempsym)))
                            (dm-v new-whole init)
                            (dm-vl whole-var new-whole 'destructuring-bind)
                            (setq whole-var new-whole))
                          (dm-v whole-var init))
                      (dm-vl (cddr v) whole-var 'destructuring-bind)))
                   (t
                    (let* ((temp (tempsym))
                           (push-val (if init (list temp init) temp)))
		      (push push-val dl)
		      (dm-vl v temp 'destructuring-bind))))))
    (let ((whole basis-form))
      (cond ((listp vldestructure)
	     (when (eq (first vldestructure) '&whole)
               (let ((named-whole (second vldestructure)))
                 (setq vldestructure (cddr vldestructure))
                 (if (listp named-whole)
                     (dm-vl named-whole whole 'destructuring-bind)
                     (setq dl (list (list named-whole whole)))))))
	    ((symbolp vldestructure)
	     (setq vldestructure (list '&rest vldestructure)))
	    (t (error "The destructuring-lambda-list ~s is not a list." vldestructure)))
      (dm-vl vldestructure whole context)
      (values whole (nreverse dl) arg-check destructure-symbols))))

;;; valid lambda-list to DEFMACRO is:
;;;
;;;	( [ &whole sym ]
;;;	  [ &environment sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
;;;
;;; where v is short for { defmacro-lambda-list | sym }.
;;; A symbol may be accepted as a DEFMACRO lambda-list, in which case
;;; (DEFMACRO <name> <symbol> ... ) is equivalent to
;;; (DEFMACRO <name> (&REST <symbol>) ...).
;;; Defmacro-lambda-list is defined as:
;;;
;;;	( { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
(defun find-documentation (body)
  ;; nth-value is not available early
  (multiple-value-bind (declarations body documentation specials)
      (process-declarations body t)
    (declare (ignore declarations body specials))
    documentation))

(defun remove-documentation (body)
  (multiple-value-bind (decls body doc)
      (process-declarations body t)
    (when decls (push `(declare ,@decls) body))
    (values body doc)))

(defun find-declarations (body &optional (docp t))
  (multiple-value-bind (decls body doc)
      (process-declarations body docp)
    (values (if decls `((declare ,@decls)) nil)
	    body doc)))

(defun function-block-name (fname)
  (etypecase fname
    (symbol fname)
    ((cons (eql setf) (cons symbol null)) (second fname))))

;; Optional argument CONTEXT can be setf-expander, type-expander,
;; or macro-function (default). This is kind of ugly because the underlying
;; DESTRUCTURE accepts instead DEFMACRO, DEFTYPE, DEFINE-COMPILER-MACRO, or
;; DESTRUCTURING-BIND, but we want fancy names.
(defun expand-defmacro (name original-lambda-list body
                        &optional (context 'cl:macro-function)
                          (block-name (function-block-name name)))
  (let ((vledm original-lambda-list))
    (multiple-value-bind (decls body doc)
        (find-declarations body)
      ;; We turn (a . b) into (a &rest b)
      ;; This is required because MEMBER (below) does not like improper lists.
      (let ((cell (last vledm)))
        (when (rest cell)
          (setq vledm (nconc (butlast vledm 0) (list '&rest (rest cell))))))
      ;; If we find an &environment variable in the lambda list,
      ;; we take note of the name and remove it from the list,
      ;; so that DESTRUCTURE does not get confused.
      (let ((env-part (member '&environment vledm :test #'eq))
            (lambda-name `(,context ,name)))
        (if env-part
            (setq vledm (nconc (ldiff vledm env-part) (cddr env-part))
                  env-part (second env-part))
            (setq env-part (gensym)
                  decls (list* `(declare (ignore ,env-part)) decls)))
        (multiple-value-bind (whole dl arg-check ignorables)
            (destructure vledm (if (eq context 'cross-clasp.clasp.ext::type-expander)
                                   'cl:deftype
                                   'cl:defmacro)
                         lambda-name name)
          (values
           `(lambda (,whole ,env-part &aux ,@dl)
              (declare (ignorable ,@ignorables)
                       (lambda-name ,lambda-name)
                       (lambda-list ,@original-lambda-list))
              ,@decls
              ,@(when doc (list doc))
              (block ,block-name
                ,@arg-check
                ,@body))
           doc))))))

;;; Like EXPAND-DEFMACRO, but is slightly nicer about invalid arguments.
(defun expand-define-compiler-macro (name vldm body
                                     &optional (block-name (function-block-name name)))
  (multiple-value-bind (decls body doc)
      (find-declarations body)
    (let ((cell (last vldm)))
      (when (rest cell)
        (setq vldm (nconc (butlast vldm 0) (list '&rest (rest cell))))))
    (let ((env-part (member '&environment vldm :test #'eq)))
      (if env-part
          (setq vldm (nconc (ldiff vldm env-part) (cddr env-part))
                env-part (second env-part))
          (setq env-part (gensym)
                decls (list* `(declare (ignore ,env-part)) decls)))
      (multiple-value-bind (whole dl arg-check ignorables)
          (destructure vldm 'cl:define-compiler-macro
                       `(compiler-macro-function ,name) name)
        (values
         `(lambda (,whole ,env-part &aux ,@dl)
            (declare (ignorable ,@ignorables)
                     (lambda-name (compiler-macro-function ,name)))
            ,@decls
            ,@(when doc (list doc))
            (block ,block-name
              (when (or ,@arg-check)
                ;; If we're here, there's been a non-constant keyword.
                ;; We can't parse that, so abandon ship.
                ;; TODO: Signal a note or something here.
                (return-from ,block-name ,whole))
              ,@body))
         doc)))))

(defun cross-clasp.clasp.ext:parse-macro (name lambda-list body &optional env)
  (declare (ignore env)) ; for now.
  (expand-defmacro name lambda-list body 'macro-function))

(defun cross-clasp.clasp.ext:parse-compiler-macro
    (name lambda-list body &optional env)
  (declare (ignore env)) ; also for now
  (expand-define-compiler-macro name lambda-list body))

(defun cross-clasp.clasp.ext:parse-deftype (name lambda-list body &optional env)
  (declare (ignore env))
  (expand-defmacro name lambda-list body 'type-expander))

(defun cross-clasp.clasp.ext:parse-define-setf-expander
    (name lambda-list body &optional env)
  (declare (ignore env))
  (expand-defmacro name lambda-list body 'setf-expander))
