;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;; -*- Package: FORMAT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Functions to implement FORMAT and FORMATTER for CMU Common Lisp.
;;;
;;; Written by William Lott, with lots of stuff stolen from the previous
;;; version by David Adam and later rewritten by Bill Maddox.
;;;
;;; Various fixes and adaptations provided by Juan Jose Garcia-Ripoll and
;;; Daniel Kochmański for Embeddable Common-Lisp.
;;; 

(in-package "SYS")

(pushnew :cdr-7 *features*)

;;;; Float printing.

;;;
;;;  Written by Bill Maddox
;;;
;;;
;;;
;;; FLONUM-TO-STRING (and its subsidiary function FLOAT-STRING) does most of 
;;; the work for all printing of floating point numbers in the printer and in
;;; FORMAT.  It converts a floating point number to a string in a free or 
;;; fixed format with no exponent.  The interpretation of the arguments is as 
;;; follows:
;;;
;;;     X        - The floating point number to convert, which must not be
;;;                negative.
;;;     WIDTH    - The preferred field width, used to determine the number
;;;                of fraction digits to produce if the FDIGITS parameter
;;;                is unspecified or NIL.  If the non-fraction digits and the
;;;                decimal point alone exceed this width, no fraction digits
;;;                will be produced unless a non-NIL value of FDIGITS has been
;;;                specified.  Field overflow is not considerd an error at this
;;;                level.
;;;     FDIGITS  - The number of fractional digits to produce. Insignificant
;;;                trailing zeroes may be introduced as needed.  May be
;;;                unspecified or NIL, in which case as many digits as possible
;;;                are generated, subject to the constraint that there are no
;;;                trailing zeroes.
;;;     SCALE    - If this parameter is specified or non-zero, then the number
;;;                printed is (* x (expt 10 scale)).  This scaling is exact,
;;;                and cannot lose precision.
;;;     FMIN     - This parameter, if specified or non-zero, is the minimum
;;;                number of fraction digits which will be produced, regardless
;;;                of the value of WIDTH or FDIGITS.  This feature is used by
;;;                the ~E format directive to prevent complete loss of
;;;                significance in the printed value due to a bogus choice of
;;;                scale factor.
;;;
;;; Most of the optional arguments are for the benefit for FORMAT and are not
;;; used by the printer.
;;;
;;; Returns:
;;; (VALUES DIGIT-STRING DIGIT-LENGTH LEADING-POINT TRAILING-POINT DECPNT)
;;; where the results have the following interpretation:
;;;
;;;     DIGIT-STRING    - The decimal representation of X, with decimal point.
;;;     DIGIT-LENGTH    - The length of the string DIGIT-STRING.
;;;     LEADING-POINT   - True if the first character of DIGIT-STRING is the
;;;                       decimal point.
;;;     TRAILING-POINT  - True if the last character of DIGIT-STRING is the
;;;                       decimal point.
;;;     POINT-POS       - The position of the digit preceding the decimal
;;;                       point.  Zero indicates point before first digit.
;;;
;;; NOTE:  FLONUM-TO-STRING goes to a lot of trouble to guarantee accuracy.
;;; Specifically, the decimal number printed is the closest possible 
;;; approximation to the true value of the binary number to be printed from 
;;; among all decimal representations  with the same number of digits.  In
;;; free-format output, i.e. with the number of digits unconstrained, it is 
;;; guaranteed that all the information is preserved, so that a properly-
;;; rounding reader can reconstruct the original binary number, bit-for-bit, 
;;; from its printed decimal representation. Furthermore, only as many digits
;;; as necessary to satisfy this condition will be printed.
;;;
;;;
;;; FLOAT-STRING actually generates the digits for positive numbers.  The
;;; algorithm is essentially that of algorithm Dragon4 in "How to Print 
;;; Floating-Point Numbers Accurately" by Steele and White.  The current 
;;; (draft) version of this paper may be found in [CMUC]<steele>tradix.press.
;;; DO NOT EVEN THINK OF ATTEMPTING TO UNDERSTAND THIS CODE WITHOUT READING 
;;; THE PAPER!

(defparameter *digits* "0123456789")

(defun float-to-digits* (digits number position relativep)
  "Does what float-to-digits does, but also detects if result is zero."
  (multiple-value-bind (exp string)
      (float-to-digits digits
                       number
                       position
                       relativep)
    (values exp
            string
            (and position
                 (< exp (- (abs position)))))))

(defun flonum-to-string (x &optional width fdigits (scale 0) (fmin 0))
  (declare (type float x))
  (if (zerop x)
      ;; Zero is a special case which FLOAT-STRING cannot handle.
      (cond ((null fdigits)  (values ".0" 2 t nil 0))
            ((zerop fdigits) (values "0." 2 nil t 1))
            (T (let ((s (make-string (1+ fdigits) :initial-element #\0)))
                 (setf (schar s 0) #\.)
                 (values s (length s) t nil 0))))
      (multiple-value-bind (e string zero?)
          (cond (fdigits
                 (float-to-digits* nil x
                                   (min (- (+ fdigits scale))
                                        (- (+ fmin scale)))
                                   nil))
                ((null width)
                 (float-to-digits* nil x nil nil))
                (T (let ((w (multiple-value-list
                             (float-to-digits* nil x
                                               (max 0
                                                    (+ (- width 2)
                                                       (if (minusp scale)
                                                           scale 0)))
                                               t)))
                         (f (multiple-value-list
                             (float-to-digits* nil x
                                               (- (+ fmin scale))
                                               nil))))
                     (if (>= (length (cadr w))
                             (length (cadr f)))
                         (values-list w)
                         (values-list f)))))
        (let* ((exp (+ e scale))
               (stream (make-string-output-stream))
               (length (length string)))
          ;; Integer part
          (when (plusp exp)
            (write-string string
                          stream
                          :end (min length exp))
            (dotimes (i (- exp length))
              (write-char #\0 stream)))
          ;; Separator and fraction
          (write-char #\. stream)
          ;; Fraction part
          (cond ((and zero? fdigits)
                 (dotimes (i fdigits)
                   (write-char #\0 stream)))
                (fdigits
                 (let ((characters-used 0))
                   (dotimes (i (min (- exp) fdigits))
                     (incf characters-used)
                     (write-char #\0 stream))
                   (let* ((start (max 0 (min length exp)))
                          (end (max start
                                    (min length
                                         (+ start (- fdigits characters-used))))))
                     (write-string string stream
                                   :start start
                                   :end   end)
                     (incf characters-used (- end start))
                     (dotimes (i (- fdigits characters-used))
                       (write-char #\0 stream)))))
                (zero?
                 (write-char #\0 stream))
                (T
                 (dotimes (i (- exp))
                   (write-char #\0 stream))
                 (let ((start (max 0 (min length exp))))
                   (write-string string stream
                                 :start start))))
          (let* ((string (get-output-stream-string stream))
                 (length (length string))
                 (position (position #\. string)))
            (values string
                    length
                    (= position 0)
                    (= position (1- length))
                    position))))))

(defun exponent-in-base10 (x)
  (if (= x 0)
      1
      (1+ (floor (log (abs x) 10)))))

(defstruct (format-directive
            (:print-function %print-format-directive))
  (string t :type simple-string)
  (start 0 :type (and unsigned-byte fixnum))
  (end 0 :type (and unsigned-byte fixnum))
  (character #\Space :type base-char)
  (colonp nil :type (member t nil))
  (atsignp nil :type (member t nil))
  (params nil :type list))

(defun %print-format-directive (struct stream depth)
  (declare (ignore depth))
  (print-unreadable-object (struct stream)
    (write-string (format-directive-string struct) stream
                  :start (format-directive-start struct)
                  :end (format-directive-end struct))))

(defconstant +format-directive-limit+ (1+ (char-code #\~)))

(defparameter *format-directive-expanders*
  (make-array +format-directive-limit+ :initial-element nil))
(defparameter *format-directive-interpreters*
  (make-array +format-directive-limit+ :initial-element nil))

(defparameter *default-format-error-control-string* nil)
(defparameter *default-format-error-offset* nil)

;; If this flag is 1, directives ~W, ~_, ~<...~:>, ~I or ~T were found.
;; If the flag is 2, directive ~<...~:;...~> was found.
;; NIL otherwise.
(defparameter *output-layout-mode* nil)

(define-condition format-error (simple-error)
  ((format-control :initarg :complaint)
   (format-arguments :initarg :arguments)
   (control-string :reader format-error-control-string
		   :initarg :control-string
		   :initform *default-format-error-control-string*) 
   (offset :reader format-error-offset :initarg :offset
	   :initform *default-format-error-offset*)
   (print-banner :reader format-error-print-banner :initarg :print-banner
		 :initform t))
  (:report (lambda (condition stream)
	     (format
              stream
              "~:[~;Error in format: ~]~
			 ~?~@[~%  ~A~%  ~V@T^~]"
              (format-error-print-banner condition)
              (simple-condition-format-control condition)
              (simple-condition-format-arguments condition)
              (format-error-control-string condition)
              (format-error-offset condition)))))


;;;; TOKENIZE-CONTROL-STRING

(defun tokenize-control-string (string)
  (declare (simple-string string))
  (let ((index 0)
        (end (length string))
        (result nil))
    (loop
      (let ((next-directive (or (position #\~ string :start index) end)))
        (when (> next-directive index)
          (push (subseq string index next-directive) result))
        (when (= next-directive end)
          (return))
        (let ((directive (parse-directive string next-directive)))
          (push directive result)
          (setf index (format-directive-end directive)))))
    (nreverse result)))

(defun parse-directive (string start)
  (declare (simple-string string))
  (let ((posn (1+ start)) (params nil) (colonp nil) (atsignp nil)
        (end (length string)))
    (flet ((get-char ()
             (if (= posn end)
                 (error 'format-error
                        :complaint "String ended before directive was found."
                        :control-string string
                        :offset start)
                 (schar string posn))))
      (loop
         (let ((char (get-char)))
           (cond ((and (not colonp) (not atsignp)
                       (or (char<= #\0 char #\9) (char= char #\+) (char= char #\-)))
                  (multiple-value-bind
                        (param new-posn)
                      (parse-integer string :start posn :junk-allowed t)
                    (push (cons posn param) params)
                    (setf posn new-posn)
                    (case (get-char)
                      (#\,)
                      ((#\: #\@)
                       (decf posn))
                      (t
                       (return)))))
                 ((and (not colonp) (not atsignp)
                       (or (char= char #\v) (char= char #\V)))
                  (push (cons posn :arg) params)
                  (incf posn)
                  (case (get-char)
                    (#\,)
                    ((#\: #\@)
                     (decf posn))
                    (t
                     (return))))
                 ((and (not colonp) (not atsignp)
                       (char= char #\#))
                  (push (cons posn :remaining) params)
                  (incf posn)
                  (case (get-char)
                    (#\,)
                    ((#\: #\@)
                     (decf posn))
                    (t
                     (return))))
                 ((and (not colonp) (not atsignp)
                       (char= char #\'))
                  (incf posn)
                  (push (cons posn (get-char)) params)
                  (incf posn)
                  (unless (char= (get-char) #\,)
                   (decf posn)))
                 ((and (not colonp) (not atsignp)
                       (char= char #\,))
                  (push (cons posn nil) params))
                 ((char= char #\:)
                  (if colonp
                      (error 'format-error
                             :complaint "Too many colons supplied."
                             :control-string string
                             :offset posn)
                      (setf colonp t)))
                 ((char= char #\@)
                  (if atsignp
                      (error 'format-error
                             :complaint "Too many at-signs supplied."
                             :control-string string
                             :offset posn)
                      (setf atsignp t)))
                 (t
                  (return))))
         (incf posn))
      (let ((char (get-char)))
        (when (char= char #\/)
          (let ((closing-slash (position #\/ string :start (1+ posn))))
            (if closing-slash
                (setf posn closing-slash)
                (error 'format-error
                       :complaint "No matching closing slash."
                       :control-string string
                       :offset posn))))
        (make-format-directive
            :string string :start start :end (1+ posn)
            :character (char-upcase char)
            :colonp colonp :atsignp atsignp
            :params (nreverse params))))))


;;;; Specials used to communicate information.

;;; *UP-UP-AND-OUT-ALLOWED* -- internal.
;;;
;;; Used both by the expansion stuff and the interpreter stuff.  When it is
;;; non-NIL, up-up-and-out (~:^) is allowed.  Otherwise, ~:^ isn't allowed.
;;;
(defparameter *up-up-and-out-allowed* nil)

;;; *LOGICAL-BLOCK-POPPER* -- internal.
;;;
;;; Used by the interpreter stuff.  When it non-NIL, its a function that will
;;; invoke PPRINT-POP in the right lexical environemnt.
;;;
(defparameter *logical-block-popper* nil)

;;; *EXPANDER-NEXT-ARG-MACRO* -- internal.
;;;
;;; Used by the expander stuff.  This is bindable so that ~<...~:>
;;; can change it.
;;;
(defparameter *expander-next-arg-macro* 'expander-next-arg)

;;; *ONLY-SIMPLE-ARGS* -- internal.
;;;
;;; Used by the expander stuff.  Initially starts as T, and gets set to NIL
;;; if someone needs to do something strange with the arg list (like use
;;; the rest, or something).
;;; 
(defvar *only-simple-args*)

;;; *ORIG-ARGS-AVAILABLE* -- internal.
;;;
;;; Used by the expander stuff.  We do an initial pass with this as NIL.
;;; If someone doesn't like this, they (throw 'need-orig-args nil) and we try
;;; again with it bound to T.  If this is T, we don't try to do anything
;;; fancy with args.
;;; 
(defparameter *orig-args-available* nil)

;;; *SIMPLE-ARGS* -- internal.
;;;
;;; Used by the expander stuff.  List of (symbol . offset) for simple args.
;;; 
(defvar *simple-args*)




;;;; FORMAT

;;#-ecl
(defun format-std (destination control-string &rest format-arguments)
  "Provides various facilities for formatting output.
  CONTROL-STRING contains a string to be output, possibly with embedded
  directives, which are flagged with the escape character \"~\".  Directives
  generally expand into additional text to be output, usually consuming one
  or more of the FORMAT-ARGUMENTS in the process.  A few useful directives
  are:
        ~A or ~nA     Prints one argument as if by PRINC
        ~S or ~nS     Prints one argument as if by PRIN1
        ~D or ~nD     Prints one argument as a decimal integer
        ~%            Does a TERPRI
        ~&            Does a FRESH-LINE

         where n is the width of the field in which the object is printed.
  
  DESTINATION controls where the result will go.  If DESTINATION is T, then
  the output is sent to the standard output stream.  If it is NIL, then the
  output is returned in a string as the value of the call.  Otherwise,
  DESTINATION must be a stream to which the output will be sent.

  Example:   (FORMAT NIL \"The answer is ~D.\" 10) => \"The answer is 10.\"

  FORMAT has many additional capabilities not described here.  Consult
  Section 22.3 (Formatted Output) of the ANSI Common Lisp standard for
  details."
  (etypecase destination
    (null
     (with-output-to-string (stream)
       (formatter-aux stream control-string format-arguments)))
    (string
     (with-output-to-string (stream destination)
       (formatter-aux stream control-string format-arguments)))
    ((member t)
     (formatter-aux *standard-output* control-string format-arguments)
     nil)
    (stream
     (formatter-aux destination control-string format-arguments)
     nil)))

(defun formatter-aux (stream string-or-fun orig-args &optional (args orig-args))
  (if (functionp string-or-fun)
      (apply string-or-fun stream args)
      (catch 'up-and-out
        (let* ((string (etypecase string-or-fun
                         (simple-string
                          string-or-fun)
                         (string
                          (coerce string-or-fun 'simple-string))))
               (*output-layout-mode* nil)
               (*default-format-error-control-string* string)
               (*logical-block-popper* nil))
          (interpret-directive-list stream (tokenize-control-string string)
                                    orig-args args)))))

(defun interpret-directive-list (stream directives orig-args args)
  (if directives
      (let ((directive (car directives)))
        (etypecase directive
          (simple-string
           (write-string directive stream)
           (interpret-directive-list stream (cdr directives) orig-args args))
          (format-directive
             (multiple-value-bind
                   (new-directives new-args)
                 (let* ((code (char-code (format-directive-character directive)))
                        (function
                         (and (< code +format-directive-limit+)
                              (svref *format-directive-interpreters* code)))
                        (*default-format-error-offset*
                         (1- (format-directive-end directive))))
                   (unless function
                     (error 'format-error
                            :complaint (format nil "Unknown format directive (~a) in (~a)." directive directives )))
                   (multiple-value-bind
                         (new-directives new-args)
                       (funcall function stream directive
                                (cdr directives) orig-args args)
                     (values new-directives new-args)))
               (interpret-directive-list stream new-directives
                                         orig-args new-args)))))
      args))


;;;; FORMATTER

(defmacro formatter (control-string)
  `#',(%formatter control-string))

(defun %formatter (control-string)
  (multiple-value-bind (guts variables)
      (%formatter-guts control-string)
    (%formatter-lambda control-string guts variables)))

(defun %formatter-lambda (control-string guts variables)
  (if (eq variables 't)
      ;; need the original args
      `(lambda (stream &rest orig-args)
         (let ((args orig-args))
           ,guts
           args))
      ;; happy day, we can use simple args
      `(lambda (stream
                &optional ,@(simple-formatter-params
                             control-string variables)
                &rest args)
         ,guts
         args)))

;;; Return (values form variables), where variables is either
;;; a list of (symbol . offset) entries, or T if orig-args are required.
(defun %formatter-guts (control-string)
  ;; First try without the original args.
  (catch 'need-orig-args
    (let* ((*simple-args* nil) (*only-simple-args* t)
           (guts (expand-control-string control-string)))
      (return-from %formatter-guts
        (values guts (nreverse *simple-args*)))))
  ;; Failing that,
  (let ((*orig-args-available* t)
        (*only-simple-args* nil))
    (values (expand-control-string control-string) t)))

(defun simple-formatter-params (control-string args)
  (mapcar (lambda (arg) (simple-formatter-param control-string arg))
          args))

;;; Return an optional parameter corresponding to a simple arg.
(defun simple-formatter-param (control-string arg)
  `(,(car arg)
    ,(simple-formatter-param-err-form control-string (cdr arg))))

(defun simple-formatter-param-err-form (control-string offset)
  `(error 'format-error
          :complaint "Required argument missing"
          :control-string ,control-string
          :offset ,offset))

(defun expand-control-string (string)
  (let* ((string (etypecase string
                   (simple-string
                    string)
                   (string
                    (coerce string 'simple-string))))
         (*output-layout-mode* nil)
         (*default-format-error-control-string* string)
         (directives (tokenize-control-string string)))
    `(block nil
       ,@(expand-directive-list directives))))

(defun expand-directive-list (directives)
  (let ((results nil)
        (remaining-directives directives))
    (loop
      (unless remaining-directives
        (return))
      (multiple-value-bind
          (form new-directives)
          (expand-directive (car remaining-directives)
                            (cdr remaining-directives))
        (when form
          (push form results))
        (setf remaining-directives new-directives)))
    (reverse results)))

(defun expand-directive (directive more-directives)
  (etypecase directive
    (simple-string
     (values `(write-string ,directive stream)
             more-directives))
    (format-directive
     (let* ((code (char-code (format-directive-character directive)))
            (expander
             (and (< code +format-directive-limit+)
                  (svref *format-directive-expanders* code)))
            (*default-format-error-offset*
             (1- (format-directive-end directive))))
       (if expander
           (funcall expander directive more-directives)
           (error 'format-error
                  :complaint "Unknown directive."))))))

(defun expand-next-arg (&optional offset)
  (if (or *orig-args-available* (not *only-simple-args*))
      `(,*expander-next-arg-macro*
        ,*default-format-error-control-string*
        ,(or offset *default-format-error-offset*))
      (let ((symbol (gensym "FORMAT-ARG-")))
        (push (cons symbol (or offset *default-format-error-offset*))
              *simple-args*)
        symbol)))


;;;; Format directive definition macros and runtime support.

(defmacro expander-next-arg (string offset)
  `(if args
       (pop args)
       (error 'format-error
              :complaint "No more arguments."
              :control-string ,string
              :offset ,offset)))

;;; NEXT-ARG -- internal.
;;;
;;; This macro is used to extract the next argument from the current arg list.
;;; This is the version used by format directive interpreters.
;;; 
(defmacro next-arg (&optional offset)
  `(progn
     (when (null args)
       (error 'format-error
              :complaint "No more arguments."
              ,@(when offset
                  `(:offset ,offset))))
     (when *logical-block-popper*
       (funcall *logical-block-popper*))
     (pop args)))

(defmacro def-complex-format-directive (char lambda-list &body body)
  (let* ((name (or (char-name char) (string char)))
         (defun-name (intern (concatenate 'string name "-FORMAT-DIRECTIVE-EXPANDER")))
         (directive (gensym))
         (directives (if lambda-list (car (last lambda-list)) (gensym))))
    `(%set-format-directive-expander 
      ,char
      #'(lambda (,directive ,directives)
          (declare (core::lambda-name ,defun-name))
          ,@(if lambda-list
                `((let ,(mapcar #'(lambda (var)
                                    `(,var
                                      (,(intern (concatenate
                                                 'string
                                                 "FORMAT-DIRECTIVE-"
                                                 (symbol-name var))
                                                (symbol-package 'foo))
                                       ,directive)))
                                (butlast lambda-list))
                    ,@body))
                `((declare (ignore ,directive ,directives))
                  ,@body))))))

(defmacro def-format-directive (char lambda-list &body body)
  (let ((directives (gensym))
        (declarations nil)
        (body-without-decls body))
    (loop
      (let ((form (car body-without-decls)))
        (unless (and (consp form) (eq (car form) 'declare))
          (return))
        (push (pop body-without-decls) declarations)))
    (setf declarations (reverse declarations))
    `(def-complex-format-directive ,char (,@lambda-list ,directives)
       ,@declarations
       (values (progn ,@body-without-decls)
               ,directives))))

(defmacro expand-bind-defaults (specs params &body body)
  (once-only ((params params))
    (if specs
        (loop for (var default) in specs
              for symbol = (gensym)
              collect `(,var ',symbol) into expander-bindings
              collect `(list ',symbol
                             (let* ((param-and-offset (pop ,params))
                                    (offset (car param-and-offset))
                                    (param (cdr param-and-offset)))
                               (case param
                                 (:arg `(or ,(expand-next-arg offset)
                                          ,,default))
                                 (:remaining
                                  (setf *only-simple-args* nil)
                                  '(length args))
                                 ((nil) ,default)
                                 (t param))))
                into runtime-bindings
              finally (return
                        `(let ,expander-bindings
                           `(let ,(list ,@runtime-bindings)
                              ,@(if ,params
                                    (error 'format-error
                                           :complaint
                                           "Too many parameters, expected no more than ~D"
                                           :arguments (list ,(length specs))
                                           :offset (caar ,params)))
                              ,,@body))))
        `(progn
           (when ,params
             (error 'format-error
                    :complaint "Too many parameters, expected no more than 0"
                    :offset (caar ,params)))
           ,@body))))

(defmacro def-complex-format-interpreter (char lambda-list &body body)
  (let* ((directive (gensym))
         (directives (if lambda-list (car (last lambda-list)) (gensym)))
         (name (or (char-name char) (string char)))
         (defun-name (intern (concatenate 'string name "-FORMAT-INTERPRETER"))))
    `(%set-format-directive-interpreter ,char
       (lambda (stream ,directive ,directives orig-args args)
         (declare (ignorable stream orig-args args) (core:lambda-name ,defun-name))
         ,@(if lambda-list
               `((let ,(mapcar #'(lambda (var)
                                   `(,var
                                     (,(intern (concatenate
                                                'string
                                                "FORMAT-DIRECTIVE-"
                                                (symbol-name var))
                                               (symbol-package 'foo))
                                      ,directive)))
                               (butlast lambda-list))
                   (values (progn ,@body) args)))
               `((declare (ignore ,directive ,directives))
                 ,@body))))))

(defmacro def-format-interpreter (char lambda-list &body body)
  (let ((directives (gensym)))
    `(def-complex-format-interpreter ,char (,@lambda-list ,directives)
       ,@body
       ,directives)))

(defmacro interpret-bind-defaults (specs params &body body)
  (once-only ((params params))
    `(let* ,(loop for (var default) in specs
                  collect `(,var (let* ((param-and-offset (pop ,params))
                                        (offset (car param-and-offset))
                                        (param (cdr param-and-offset)))
                                   (case param
                                     (:arg (or (next-arg offset) ,default))
                                     (:remaining (length args))
                                     ((nil) ,default)
                                     (t param)))))
       (when ,params
         (error 'format-error
                :complaint
                "Too many parameters, expected no more than ~D"
                :arguments (list ,(length specs))
                :offset (caar ,params)))
       ,@body)))

(defun %set-format-directive-expander (char fn)
  (setf (aref *format-directive-expanders* (char-code (char-upcase char))) fn)
  char)

(defun %set-format-directive-interpreter (char fn)
  (setf (aref *format-directive-interpreters*
              (char-code (char-upcase char)))
        fn)
  char)

(defun find-directive (directives kind stop-at-semi)
  (if directives
      (let ((next (car directives)))
        (if (format-directive-p next)
            (let ((char (format-directive-character next)))
              (if (or (char= kind char)
                      (and stop-at-semi (char= char #\;)))
                  (car directives)
                  (find-directive
                   (cdr (flet ((after (char)
                                 (member (find-directive (cdr directives)
                                                         char
                                                         nil)
                                         directives)))
                          (case char
                            (#\( (after #\)))
                            (#\< (after #\>))
                            (#\[ (after #\]))
                            (#\{ (after #\}))
                            (t directives))))
                   kind stop-at-semi)))
            (find-directive (cdr directives) kind stop-at-semi)))))


;;;; Simple outputting noise.

(defun format-write-field (stream string mincol colinc minpad padchar padleft)
  (unless padleft
    (write-string string stream))
  (setf minpad (max minpad 0))
  (dotimes (i minpad)
    (write-char padchar stream))
  (and mincol minpad colinc
       (do ((chars (+ (length string) minpad) (+ chars colinc)))
           ((>= chars mincol))
         (dotimes (i colinc)
           (write-char padchar stream))))
  (when padleft
    (write-string string stream)))

(defun format-princ (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
                      (if (or arg (not colonp))
                          (princ-to-string arg)
                          "()")
                      mincol colinc minpad padchar atsignp))

(def-format-directive #\A (colonp atsignp params)
  (if params
      (expand-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
                             (padchar #\space))
                     params
        `(format-princ stream ,(expand-next-arg) ',colonp ',atsignp
                       ,mincol ,colinc ,minpad ,padchar))
      `(princ ,(if colonp
                   `(or ,(expand-next-arg) "()")
                   (expand-next-arg))
              stream)))

(def-format-interpreter #\A (colonp atsignp params)
  (if params
      (interpret-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
                                (padchar #\space))
                     params
        (format-princ stream (next-arg) colonp atsignp
                      mincol colinc minpad padchar))
      (princ (if colonp (or (next-arg) "()") (next-arg)) stream)))

(defun format-prin1 (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
                      (if (or arg (not colonp))
                          (prin1-to-string arg)
                          "()")
                      mincol colinc minpad padchar atsignp))

(def-format-directive #\S (colonp atsignp params)
  (cond (params
         (expand-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
                                (padchar #\space))
                        params
           `(format-prin1 stream ,(expand-next-arg) ,colonp ,atsignp
                          ,mincol ,colinc ,minpad ,padchar)))
        (colonp
         `(let ((arg ,(expand-next-arg)))
            (if arg
                (prin1 arg stream)
                (princ "()" stream))))
        (t
         `(prin1 ,(expand-next-arg) stream))))

(def-format-interpreter #\S (colonp atsignp params)
  (cond (params
         (interpret-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
                                   (padchar #\space))
                        params
           (format-prin1 stream (next-arg) colonp atsignp
                         mincol colinc minpad padchar)))
        (colonp
         (let ((arg (next-arg)))
           (if arg
               (prin1 arg stream)
               (princ "()" stream))))
        (t
         (prin1 (next-arg) stream))))

(def-format-directive #\C (colonp atsignp params)
  (expand-bind-defaults () params
    (if colonp
        `(format-print-named-character ,(expand-next-arg) stream)
        (if atsignp
            `(prin1 ,(expand-next-arg) stream)
            `(write-char ,(expand-next-arg) stream)))))

(def-format-interpreter #\C (colonp atsignp params)
  (interpret-bind-defaults () params
    (if colonp
        (format-print-named-character (next-arg) stream)
        (if atsignp
            (prin1 (next-arg) stream)
            (write-char (next-arg) stream)))))

(defun format-print-named-character (char stream)
  (if (printing-char-p char)
      (write-char char stream)
      (write-string (char-name char) stream)))

(def-format-directive #\W (colonp atsignp params)
  (check-output-layout-mode 1)
  (expand-bind-defaults () params
    (if (or colonp atsignp)
        `(let (,@(when colonp
                   '((*print-pretty* t)))
               ,@(when atsignp
                   '((*print-level* nil)
                     (*print-length* nil))))
           (write-object ,(expand-next-arg) stream))
        `(write-object ,(expand-next-arg) stream))))

(def-format-interpreter #\W (colonp atsignp params)
  (check-output-layout-mode 1)
  (interpret-bind-defaults () params
    (let ((*print-pretty* (or colonp *print-pretty*))
          (*print-level* (and atsignp *print-level*))
          (*print-length* (and atsignp *print-length*)))
      (write-object (next-arg) stream))))


;;;; Integer outputting.

;;; FORMAT-PRINT-NUMBER does most of the work for the numeric printing
;;; directives.  The parameters are interpreted as defined for ~D.
;;;
(defun format-print-integer (stream number print-commas-p print-sign-p
                             radix mincol padchar commachar commainterval)
 (let ((*print-base* radix)
        (*print-radix* nil))
    (if (integerp number)
        (let* ((text (princ-to-string (abs number)))
               (commaed (if print-commas-p
                            (format-add-commas text commachar commainterval)
                            text))
               (signed (cond ((minusp number)
                              (concatenate 'string "-" commaed))
                             (print-sign-p
                              (concatenate 'string "+" commaed))
                             (t commaed))))
          ;; colinc = 1, minpad = 0, padleft = t
          (format-write-field stream signed mincol 1 0 padchar t))
        (princ number stream))))

(defun format-add-commas (string commachar commainterval)
  (let ((length (length string)))
    (multiple-value-bind (commas extra)
                         (truncate (1- length) commainterval)
      (let ((new-string (make-string (+ length commas)))
            (first-comma (1+ extra)))
        (replace new-string string :end1 first-comma :end2 first-comma)
        (do ((src first-comma (+ src commainterval))
             (dst first-comma (+ dst commainterval 1)))
            ((= src length))
          (setf (schar new-string dst) commachar)
          (replace new-string string :start1 (1+ dst)
                   :start2 src :end2 (+ src commainterval)))
        new-string))))

(defun expand-format-integer (base colonp atsignp params)
  (if (or colonp atsignp params)
      (expand-bind-defaults
          ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
          params
        `(format-print-integer stream ,(expand-next-arg) ,colonp ,atsignp
                               ,base ,mincol ,padchar ,commachar
                               ,commainterval))
      `(write ,(expand-next-arg) :stream stream :base ,base :radix nil
              :escape nil)))

(eval-when (:compile-toplevel :execute)
(defmacro interpret-format-integer (base)
  `(if (or colonp atsignp params)
       (interpret-bind-defaults
           ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
           params
         (format-print-integer stream (next-arg) colonp atsignp ,base mincol
                               padchar commachar commainterval))
       (write (next-arg) :stream stream :base ,base :radix nil :escape nil)))
)

(def-format-directive #\D (colonp atsignp params)
  (expand-format-integer 10 colonp atsignp params))

(def-format-interpreter #\D (colonp atsignp params)
  (interpret-format-integer 10))

(def-format-directive #\B (colonp atsignp params)
  (expand-format-integer 2 colonp atsignp params))

(def-format-interpreter #\B (colonp atsignp params)
  (interpret-format-integer 2))

(def-format-directive #\O (colonp atsignp params)
  (expand-format-integer 8 colonp atsignp params))

(def-format-interpreter #\O (colonp atsignp params)
  (interpret-format-integer 8))

(def-format-directive #\X (colonp atsignp params)
  (expand-format-integer 16 colonp atsignp params))

(def-format-interpreter #\X (colonp atsignp params)
  (interpret-format-integer 16))

(def-format-directive #\R (colonp atsignp params)
  (expand-bind-defaults
      ((base nil) (mincol 0) (padchar #\space) (commachar #\,)
       (commainterval 3))
      params
    (let ((n-arg (gensym)))
      `(let ((,n-arg ,(expand-next-arg)))
         (if ,base
             (format-print-integer stream ,n-arg ,colonp ,atsignp
                                   ,base ,mincol
                                   ,padchar ,commachar ,commainterval)
             ,(if atsignp
                  (if colonp
                      `(format-print-old-roman stream ,n-arg)
                      `(format-print-roman stream ,n-arg))
                  (if colonp
                      `(format-print-ordinal stream ,n-arg)
                      `(format-print-cardinal stream ,n-arg))))))))

(def-format-interpreter #\R (colonp atsignp params)
  (interpret-bind-defaults
      ((base nil) (mincol 0) (padchar #\space) (commachar #\,)
       (commainterval 3))
      params
    (if base
       (format-print-integer stream (next-arg) colonp atsignp base mincol
                             padchar commachar commainterval)
       (if atsignp
           (if colonp
               (format-print-old-roman stream (next-arg))
               (format-print-roman stream (next-arg)))
           (if colonp
               (format-print-ordinal stream (next-arg))
               (format-print-cardinal stream (next-arg)))))))

(defconstant-eqx cardinal-ones
    #(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")
  equalp)

(defconstant-eqx cardinal-tens
    #(nil nil "twenty" "thirty" "forty"
      "fifty" "sixty" "seventy" "eighty" "ninety")
  equalp)

(defconstant-eqx cardinal-teens
    #("ten" "eleven" "twelve" "thirteen" "fourteen"  ;;; RAD
      "fifteen" "sixteen" "seventeen" "eighteen" "nineteen")
  equalp)

(defconstant-eqx cardinal-periods
    #("" " thousand" " million" " billion" " trillion" " quadrillion"
      " quintillion" " sextillion" " septillion" " octillion" " nonillion"
      " decillion" " undecillion" " duodecillion" " tredecillion"
      " quattuordecillion" " quindecillion" " sexdecillion" " septendecillion"
      " octodecillion" " novemdecillion" " vigintillion")
  equalp)

(defconstant-eqx ordinal-ones
    #(nil "first" "second" "third" "fourth"
      "fifth" "sixth" "seventh" "eighth" "ninth")
  equalp
  "Table of ordinal ones-place digits in English")

(defconstant-eqx ordinal-tens 
    #(nil "tenth" "twentieth" "thirtieth" "fortieth"
      "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth")
  equalp
  "Table of ordinal tens-place digits in English")

(defun format-print-small-cardinal (stream n)
  (multiple-value-bind 
      (hundreds rem) (truncate n 100)
    (when (plusp hundreds)
      (write-string (svref cardinal-ones hundreds) stream)
      (write-string " hundred" stream)
      (when (plusp rem)
        (write-char #\space stream)))
    (when (plusp rem)
      (multiple-value-bind (tens ones)
                           (truncate rem 10)
       (cond ((< 1 tens)
              (write-string (svref cardinal-tens tens) stream)
              (when (plusp ones)
                (write-char #\- stream)
                (write-string (svref cardinal-ones ones) stream)))
             ((= tens 1)
              (write-string (svref cardinal-teens ones) stream))
             ((plusp ones)
              (write-string (svref cardinal-ones ones) stream)))))))

(defun format-print-cardinal (stream n)
  (cond ((minusp n)
         (write-string "negative " stream)
         (format-print-cardinal-aux stream (- n) 0 n))
        ((zerop n)
         (write-string "zero" stream))
        (t
         (format-print-cardinal-aux stream n 0 n))))

(defun format-print-cardinal-aux (stream n period err)
  (multiple-value-bind (beyond here) (truncate n 1000)
    (unless (<= period 20)
      (error "Number too large to print in English: ~:D" err))
    (unless (zerop beyond)
      (format-print-cardinal-aux stream beyond (1+ period) err))
    (unless (zerop here)
      (unless (zerop beyond)
        (write-char #\space stream))
      (format-print-small-cardinal stream here)
      (write-string (svref cardinal-periods period) stream))))

(defun format-print-ordinal (stream n)
  (when (minusp n)
    (write-string "negative " stream))
  (let ((number (abs n)))
    (multiple-value-bind
        (top bot) (truncate number 100)
      (unless (zerop top)
        (format-print-cardinal stream (- number bot)))
      (when (and (plusp top) (plusp bot))
        (write-char #\space stream))
      (multiple-value-bind
          (tens ones) (truncate bot 10)
        (cond ((= bot 12) (write-string "twelfth" stream))
              ((= tens 1)
               (write-string (svref cardinal-teens ones) stream);;;RAD
               (write-string "th" stream))
              ((and (zerop tens) (plusp ones))
               (write-string (svref ordinal-ones ones) stream))
              ((and (zerop ones)(plusp tens))
               (write-string (svref ordinal-tens tens) stream))
              ((plusp bot)
               (write-string (svref cardinal-tens tens) stream)
               (write-char #\- stream)
               (write-string (svref ordinal-ones ones) stream))
              ((plusp number)
               (write-string "th" stream))
              (t
               (write-string "zeroth" stream)))))))

;;; Print Roman numerals

(defun format-print-old-roman (stream n)
  (unless (< 0 n 5000)
    (error "Number too large to print in old Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (start n (do ((i start (progn
                                (write-char cur-char stream)
                                (- i cur-val))))
                    ((< i cur-val) i))))
      ((zerop start))))

(defun format-print-roman (stream n)
  (unless (< 0 n 4000)
    (error "Number too large to print in Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (sub-chars '(#\C #\X #\X #\I #\I) (cdr sub-chars))
       (sub-val '(100 10 10 1 1 0) (cdr sub-val))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (cur-sub-char #\C (car sub-chars))
       (cur-sub-val 100 (car sub-val))
       (start n (do ((i start (progn
                                (write-char cur-char stream)
                                (- i cur-val))))
                    ((< i cur-val)
                     (cond ((<= (- cur-val cur-sub-val) i)
                            (write-char cur-sub-char stream)
                            (write-char cur-char stream)
                            (- i (- cur-val cur-sub-val)))
                           (t i))))))
          ((zerop start))))


;;;; Plural.

(def-format-directive #\P (colonp atsignp params end)
  (expand-bind-defaults () params
    (let ((arg (cond
                ((not colonp)
                 (expand-next-arg))
                (*orig-args-available*
                 `(if (eq orig-args args)
                      (error 'format-error
                             :complaint "No previous argument."
                             :offset ,(1- end))
                      (do ((arg-ptr orig-args (cdr arg-ptr)))
                          ((eq (cdr arg-ptr) args)
                           (car arg-ptr)))))
                (*only-simple-args*
                 (unless *simple-args*
                   (error 'format-error
                          :complaint "No previous argument."))
                 (caar *simple-args*))
                (t
                 (throw 'need-orig-args nil)))))
      (if atsignp
          `(write-string (if (eql ,arg 1) "y" "ies") stream)
          `(unless (eql ,arg 1) (write-char #\s stream))))))

(def-format-interpreter #\P (colonp atsignp params)
  (interpret-bind-defaults () params
    (let ((arg (if colonp
                   (if (eq orig-args args)
                       (error 'format-error
                              :complaint "No previous argument.")
                       (do ((arg-ptr orig-args (cdr arg-ptr)))
                           ((eq (cdr arg-ptr) args)
                            (car arg-ptr))))
                   (next-arg))))
      (if atsignp
          (write-string (if (eql arg 1) "y" "ies") stream)
          (unless (eql arg 1) (write-char #\s stream))))))


;;;; Floating point noise.

(defun decimal-string (n)
  (write-to-string n :base 10 :radix nil :escape nil))

(def-format-directive #\F (colonp atsignp params)
  (when colonp
    (error 'format-error
           :complaint
           "Cannot specify the colon modifier with this directive."))
  (expand-bind-defaults ((w nil) (d nil) (k 0) (ovf nil) (pad #\space)) params
                        `(format-fixed stream ,(expand-next-arg) ,w ,d ,k ,ovf ,pad ,atsignp)))

(def-format-interpreter #\F (colonp atsignp params)
  (when colonp
    (error 'format-error
           :complaint
           "Cannot specify the colon modifier with this directive."))
  (interpret-bind-defaults ((w nil) (d nil) (k 0) (ovf nil) (pad #\space))
                           params
                           (format-fixed stream (next-arg) w d k ovf pad atsignp)))

(defun format-fixed (stream number w d k ovf pad atsign)
  (if (numberp number)
      (if (floatp number)
          (format-fixed-aux stream number w d k ovf pad atsign)
          (if (rationalp number)
              (format-fixed-aux stream
                                (coerce number 'single-float)
                                w d k ovf pad atsign)
              (format-write-field stream
                                  (decimal-string number)
                                  w 1 0 #\space t)))
      (format-princ stream number nil nil w 1 0 pad)))


;;; We return true if we overflowed, so that ~G can output the overflow char
;;; instead of spaces.
;;;
(defun format-fixed-aux (stream number w d k ovf pad atsign)
  (cond
    ((or (not (or w d k))
         (non-finite-float-p number))
     (prin1 number stream)
     nil)
    (t
     (let ((spaceleft w))
       (when (and w (or atsign
                        (minusp number)
                        #+ieee-floating-point
                        (and (zerop number)
                             (minusp (atan number -1)))))
         (decf spaceleft))
       (multiple-value-bind (str len lpoint tpoint)
           (sys::flonum-to-string (abs number) spaceleft d k)
         ;; if caller specifically requested no fraction digits, suppress the
         ;; trailing zero
         (when (eql d 0)
           (setq tpoint nil))
         (when w 
           (decf spaceleft len)
           ;; obligatory trailing zero (unless explicitly cut with ,d)
           (when tpoint
             (decf spaceleft))
           ;; optional leading zero
           (when lpoint
             (if (or (> spaceleft 0)
                     (eql d 0))
                 (decf spaceleft)
                 (setq lpoint nil))))
         (cond ((and w (< spaceleft 0) ovf)
                ;;field width overflow
                (dotimes (i w)
                  (write-char ovf stream))
                t)
               (t
                (when w (dotimes (i spaceleft) (write-char pad stream)))
                (if (or (minusp number)
                        (and (zerop number)
                             (minusp (atan number -1))))
                    (write-char #\- stream)
                    (if atsign (write-char #\+ stream)))
                (when lpoint (write-char #\0 stream))
                (write-string str stream)
                (when tpoint (write-char #\0 stream))
                nil)))))))

(def-format-directive #\E (colonp atsignp params)
  (when colonp
    (error 'format-error
           :complaint
           "Cannot specify the colon modifier with this directive."))
  (expand-bind-defaults
      ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (mark nil))
      params
    `(format-exponential stream ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark
                         ,atsignp)))

(def-format-interpreter #\E (colonp atsignp params)
  (when colonp
    (error 'format-error
           :complaint
           "Cannot specify the colon modifier with this directive."))
  (interpret-bind-defaults
      ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (mark nil))
      params
    (format-exponential stream (next-arg) w d e k ovf pad mark atsignp)))

(defun format-exponential (stream number w d e k ovf pad marker atsign)
  (cond
    ((not (numberp number))
     (format-princ stream number nil nil w 1 0 pad))
    ((floatp number)
     (format-exp-aux stream number w d e k ovf pad marker atsign))
    ((rationalp number)
     (format-exp-aux stream
                     (coerce number 'single-float)
                     w d e k ovf pad marker atsign))
    (T
     (format-write-field stream
                         (decimal-string number)
                         w 1 0 #\space t))))

(defun format-exponent-marker (number)
  (if (typep number *read-default-float-format*)
      #\e
      (typecase number
        (single-float #\f)
        (double-float #\d)
        (short-float #\s)
        (long-float #\l))))

(defun non-finite-float-p (number)
  ;; FIXME: Get infinityp and nanp predicates.
  ;; numbers.h has them, but only for singles.
  #-(or) (declare (ignore number))
  #+(or)
  (and (floatp number)
       (or (float-infinity-p number)
           (float-nan-p number)))
  #-(or) nil)

;;;Here we prevent the scale factor from shifting all significance out of
;;;a number to the right.  We allow insignificant zeroes to be shifted in
;;;to the left right, athough it is an error to specify k and d such that this
;;;occurs.  Perhaps we should detect both these condtions and flag them as
;;;errors.  As for now, we let the user get away with it, and merely guarantee
;;;that at least one significant digit will appear.

;;; toy@rtp.ericsson.se:  The Hyperspec seems to say that the exponent
;;; marker is always printed.  Make it so.  Also, the original version
;;; causes errors when printing infinities or NaN's.  The Hyperspec is
;;; silent here, so let's just print out infinities and NaN's instead
;;; of causing an error.

(defun format-exp-aux (stream number w d e k ovf pad marker atsign)
  (if (non-finite-float-p number)
      (prin1 number stream)
      (let* ((expt (- (exponent-in-base10 number) k))
             (estr (decimal-string (abs expt)))
             (elen (if e (max (length estr) e) (length estr)))
             (fdig (if d (if (plusp k) (1+ (- d k)) d) nil))
             (fmin (if (minusp k) (- 1 k) 0))
             (spaceleft (if w
                            (- w 2 elen
                               (if (or atsign (minusp number))
                                   1 0))
                            nil)))
        (if (and w ovf e (> elen e))   ;exponent overflow
            (dotimes (i w) (write-char ovf stream))
            (multiple-value-bind (fstr flen lpoint tpoint dpos)
                (sys::flonum-to-string number spaceleft fdig (- expt) fmin)
              (when (and (plusp k)
                         (< k dpos))
                (incf expt (- dpos k))
                (setf estr (decimal-string (abs expt))
                      tpoint nil)
                (loop for pos from dpos downto k
                      do (setf (char fstr pos) (if (= pos k) #\. (char fstr (1- pos))))))
              (when (eql fdig 0)
                (setq tpoint nil))
              (when w
                (decf spaceleft flen)
                (when lpoint
                  (if (> spaceleft 0)
                      (decf spaceleft)
                      (setq lpoint nil)))
                (when tpoint
                  (if (> spaceleft 0)
                      (decf spaceleft)
                      (setq tpoint nil))))
              (cond ((and w (< spaceleft 0) ovf)
                     ;;significand overflow
                     (dotimes (i w) (write-char ovf stream)))
                    (t (when w
                         (dotimes (i spaceleft) (write-char pad stream)))
                       (if (minusp number)
                           (write-char #\- stream)
                           (if atsign (write-char #\+ stream)))
                       (when lpoint (write-char #\0 stream))
                       (write-string fstr stream)
                       (when tpoint (write-char #\0 stream))
                       (write-char (if marker
                                       marker
                                       (format-exponent-marker number))
                                   stream)
                       (write-char (if (minusp expt) #\- #\+) stream)
                       (when e
                         ;;zero-fill before exponent if necessary
                         (dotimes (i (- e (length estr)))
                           (write-char #\0 stream)))
                       (write-string estr stream))))))))

(def-format-directive #\G (colonp atsignp params)
  (when colonp
    (error 'format-error
           :complaint
           "Cannot specify the colon modifier with this directive."))
  (expand-bind-defaults
   ((w nil) (d nil) (e nil) (k 0) (ovf nil) (pad #\space) (mark nil))
   params
   `(format-general stream ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark ,atsignp)))

(def-format-interpreter #\G (colonp atsignp params)
  (when colonp
    (error 'format-error
           :complaint
           "Cannot specify the colon modifier with this directive."))
  (interpret-bind-defaults
   ((w nil) (d nil) (e nil) (k 0) (ovf nil) (pad #\space) (mark nil))
   params
   (format-general stream (next-arg) w d e k ovf pad mark atsignp)))

(defun format-general (stream number w d e k ovf pad marker atsign)
  (if (numberp number)
      (if (floatp number)
          (format-general-aux stream number w d e k ovf pad marker atsign)
          (if (rationalp number)
              (format-general-aux stream
                                  (coerce number 'single-float)
                                  w d e k ovf pad marker atsign)
              (format-write-field stream
                                  (decimal-string number)
                                  w 1 0 #\space t)))
      (format-princ stream number nil nil w 1 0 pad)))


;;; toy@rtp.ericsson.se:  Same change as for format-exp-aux.
(defun format-general-aux (stream number w d e k ovf pad marker atsign)
  (if (non-finite-float-p number)
      (prin1 number stream)
      (let ((n (sys::exponent-in-base10 number)))
        ;;Default d if omitted.  The procedure is taken directly
        ;;from the definition given in the manual, and is not
        ;;very efficient, since we generate the digits twice.
        ;;Future maintainers are encouraged to improve on this.
        (unless d
          (multiple-value-bind (str len) 
              (sys::flonum-to-string (abs number))
            (declare (ignore str))
            (let ((q (if (= len 1) 1 (1- len))))
              (setq d (max q (min n 7))))))
        (let* ((ee (if e (+ e 2) 4))
               (ww (if w (- w ee) nil))
               (dd (- d n)))
          (cond ((<= 0 dd d)
                 (let ((char (if (format-fixed-aux stream number ww dd 0
                                                   ovf pad atsign)
                                 ovf
                                 #\space)))
                   (dotimes (i ee) (write-char char stream))))
                (t
                 (format-exp-aux stream number w d e (or k 1)
                                 ovf pad marker atsign)))))))

(def-format-directive #\$ (colonp atsignp params)
  (expand-bind-defaults ((d 2) (n 1) (w 0) (pad #\space)) params
    `(format-dollars stream ,(expand-next-arg) ,d ,n ,w ,pad ,colonp
                     ,atsignp)))

(def-format-interpreter #\$ (colonp atsignp params)
  (interpret-bind-defaults ((d 2) (n 1) (w 0) (pad #\space)) params
    (format-dollars stream (next-arg) d n w pad colonp atsignp)))

(defun format-dollars (stream number d n w pad colon atsign)
  (if (rationalp number) (setq number (coerce number 'single-float)))
  (if (floatp number)
      (let* ((signstr (if (minusp number) "-" (if atsign "+" "")))
             (signlen (length signstr)))
        (multiple-value-bind (str strlen ig2 ig3 pointplace)
            (sys::flonum-to-string (abs number) nil d)
          (declare (ignore ig2 ig3))
          (when colon (write-string signstr stream))
          (dotimes (i (- w signlen (max 0 (- n pointplace)) strlen))
            (write-char pad stream))
          (unless colon (write-string signstr stream))
          (dotimes (i (- n pointplace)) (write-char #\0 stream))
          (write-string str stream)))
      (format-write-field stream
                          (decimal-string number)
                          w 1 0 #\space t)))


;;;; line/page breaks and other stuff like that.

(def-format-directive #\% (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify either colon or atsign for this directive."))
  (if params
      (expand-bind-defaults ((count 1)) params
        `(dotimes (i ,count)
           (terpri stream)))
      '(terpri stream)))

(def-format-interpreter #\% (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify either colon or atsign for this directive."))
  (interpret-bind-defaults ((count 1)) params
    (dotimes (i count)
      (terpri stream))))

(def-format-directive #\& (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify either colon or atsign for this directive."))
  (if params
      (expand-bind-defaults ((count 1)) params
        `(progn
           (fresh-line stream)
           (dotimes (i (1- ,count))
             (terpri stream))))
      '(fresh-line stream)))

(def-format-interpreter #\& (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify either colon or atsign for this directive."))
  (interpret-bind-defaults ((count 1)) params
    (fresh-line stream)
    (dotimes (i (1- count))
      (terpri stream))))

(def-format-directive #\| (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify either colon or atsign for this directive."))
  (if params
      (expand-bind-defaults ((count 1)) params
        `(dotimes (i ,count)
           (write-char #\page stream)))
      '(write-char #\page stream)))

(def-format-interpreter #\| (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify either colon or atsign for this directive."))
  (interpret-bind-defaults ((count 1)) params
    (dotimes (i count)
      (write-char #\page stream))))

(def-format-directive #\~ (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify either colon or atsign for this directive."))
  (if params
      (expand-bind-defaults ((count 1)) params
        `(dotimes (i ,count)
           (write-char #\~ stream)))
      '(write-char #\~ stream)))

(def-format-interpreter #\~ (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify either colon or atsign for this directive."))
  (interpret-bind-defaults ((count 1)) params
    (dotimes (i count)
      (write-char #\~ stream))))

(def-complex-format-directive #\newline (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify both colon and atsign for this directive."))
  (values (expand-bind-defaults () params
            (if atsignp
                '(write-char #\newline stream)
                nil))
          (if (and (not colonp)
                   directives
                   (simple-string-p (car directives)))
              (cons (string-left-trim '(#\space #\newline #\tab)
                                      (car directives))
                    (cdr directives))
              directives)))

(def-complex-format-interpreter #\newline (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify both colon and atsign for this directive."))
  (interpret-bind-defaults () params
    (when atsignp
      (write-char #\newline stream)))
  (if (and (not colonp)
           directives
           (simple-string-p (car directives)))
      (cons (string-left-trim '(#\space #\newline #\tab)
                              (car directives))
            (cdr directives))
      directives))

(def-complex-format-directive #\return (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify both colon and atsign for this directive."))
  (values (expand-bind-defaults () params
            (if atsignp
                '(write-char #\newline stream)
                nil))
          (if (and (not colonp)
                   directives
                   (simple-string-p (car directives)))
              (cons (string-left-trim '(#\space #\newline #\tab)
                                      (car directives))
                    (cdr directives))
              directives)))

(def-complex-format-interpreter #\return (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify both colon and atsign for this directive."))
  (interpret-bind-defaults () params
    (when atsignp
      (write-char #\newline stream)))
  (if (and (not colonp)
           directives
           (simple-string-p (car directives)))
      (cons (string-left-trim '(#\space #\newline #\tab)
                              (car directives))
            (cdr directives))
      directives))

;;;; *

(def-format-directive #\* (colonp atsignp params end)
  (if atsignp
      (if colonp
          (error 'format-error
                 :complaint "Cannot specify both colon and at-sign.")
          (expand-bind-defaults ((posn 0)) params
            (unless *orig-args-available*
              (throw 'need-orig-args nil))
            `(if (<= 0 ,posn (length orig-args))
                 (setf args (nthcdr ,posn orig-args))
                 (error 'format-error
                        :complaint "Index ~D out of bounds.  Should have been ~
                                    between 0 and ~D."
                        :arguments (list ,posn (length orig-args))
                        :offset ,(1- end)))))
      (if colonp
          (expand-bind-defaults ((n 1)) params
            (unless *orig-args-available*
              (throw 'need-orig-args nil))
            `(do ((cur-posn 0 (1+ cur-posn))
                  (arg-ptr orig-args (cdr arg-ptr)))
                 ((eq arg-ptr args)
                  (let ((new-posn (- cur-posn ,n)))
                    (if (<= 0 new-posn (length orig-args))
                        (setf args (nthcdr new-posn orig-args))
                        (error 'format-error
                               :complaint
                               "Index ~D out of bounds.  Should have been ~
                                between 0 and ~D."
                               :arguments
                               (list new-posn (length orig-args))
                               :offset ,(1- end)))))))
          (if params
              (expand-bind-defaults ((n 1)) params
                (setf *only-simple-args* nil)
                `(dotimes (i ,n)
                   ,(expand-next-arg)))
              (expand-next-arg)))))

(def-format-interpreter #\* (colonp atsignp params)
  (if atsignp
      (if colonp
          (error 'format-error
                 :complaint "Cannot specify both colon and at-sign.")
          (interpret-bind-defaults ((posn 0)) params
            (if (<= 0 posn (length orig-args))
                (setf args (nthcdr posn orig-args))
                (error 'format-error
                       :complaint "Index ~D out of bounds.  Should have been ~
                                   between 0 and ~D."
                       :arguments (list posn (length orig-args))))))
      (if colonp
          (interpret-bind-defaults ((n 1)) params
            (do ((cur-posn 0 (1+ cur-posn))
                 (arg-ptr orig-args (cdr arg-ptr)))
                ((eq arg-ptr args)
                 (let ((new-posn (- cur-posn n)))
                   (if (<= 0 new-posn (length orig-args))
                       (setf args (nthcdr new-posn orig-args))
                       (error 'format-error
                              :complaint
                              "Index ~D out of bounds.  Should have been ~
                               between 0 and ~D."
                              :arguments
                              (list new-posn (length orig-args))))))))
          (interpret-bind-defaults ((n 1)) params
            (dotimes (i n)
              (next-arg))))))


;;;; Indirection.

(def-format-directive #\? (colonp atsignp params string end)
  (when colonp
    (error 'format-error
           :complaint "Cannot specify the colon modifier."))
  (expand-bind-defaults () params
    `(handler-bind
         ((format-error
           #'(lambda (condition)
               (error 'format-error
                      :complaint
                      "~A~%while processing indirect format string:"
                      :arguments (list condition)
                      :print-banner nil
                      :control-string ,string
                      :offset ,(1- end)))))
       ,(if atsignp
            (if *orig-args-available*
                `(setf args (formatter-aux stream ,(expand-next-arg) orig-args args))
                (throw 'need-orig-args nil))
            `(formatter-aux stream ,(expand-next-arg) ,(expand-next-arg))))))

(def-format-interpreter #\? (colonp atsignp params string end)
  (when colonp
    (error 'format-error
           :complaint "Cannot specify the colon modifier."))
  (interpret-bind-defaults () params
    (handler-bind
        ((format-error
          #'(lambda (condition)
              (error 'format-error
                     :complaint
                     "~A~%while processing indirect format string:"
                     :arguments (list condition)
                     :print-banner nil
                     :control-string string
                     :offset (1- end)))))
      (if atsignp
          (setf args (formatter-aux stream (next-arg) orig-args args))
          (formatter-aux stream (next-arg) (next-arg))))))


;;;; Capitalization.

(defun nstring-capitalize-first (s)
  (nstring-downcase s)
  (let ((where (position-if #'alpha-char-p s)))
    (when where
      (nstring-capitalize s :start 0 :end (1+ where)))
    s))

(def-complex-format-directive #\( (colonp atsignp params directives)
  (let ((close (find-directive directives #\) nil)))
    (unless close
      (error 'format-error
             :complaint "No corresponding close paren."))
    (let* ((posn (position close directives))
           (before (subseq directives 0 posn))
           (after (nthcdr (1+ posn) directives)))
      (values
       (expand-bind-defaults () params
         #-(or ecl clasp)
         `(let ((stream (make-case-frob-stream stream
                                               ,(if colonp
                                                    (if atsignp
                                                        :upcase
                                                        :capitalize)
                                                    (if atsignp
                                                        :capitalize-first
                                                        :downcase)))))
            ,@(expand-directive-list before))
         #+(or ecl clasp)
         `(let ((string (make-array 10 :element-type 'character
                                       :fill-pointer 0 :adjustable t)))
            (unwind-protect
                 (with-output-to-string (stream string)
                   ,@(expand-directive-list before))
              (princ (,(if colonp
                           (if atsignp 'nstring-upcase 'nstring-capitalize)
                           (if atsignp 'nstring-capitalize-first 'nstring-downcase))
                       string)
                     stream))))
       after))))

(def-complex-format-interpreter #\( (colonp atsignp params directives)
  (let ((close (find-directive directives #\) nil)))
    (unless close
      (error 'format-error
             :complaint "No corresponding close paren."))
    (interpret-bind-defaults () params
      #-(or ecl clasp)
      (let* ((posn (position close directives))
             (before (subseq directives 0 posn))
             (after (nthcdr (1+ posn) directives))
             (stream (make-case-frob-stream stream
                                            (if colonp
                                                (if atsignp
                                                    :upcase
                                                    :capitalize)
                                                (if atsignp
                                                    :capitalize-first
                                                    :downcase)))))
        (setf args (interpret-directive-list stream before orig-args args))
        after)
      #+(or ecl clasp)
      (let* ((posn (position close directives))
             (before (subseq directives 0 posn))
             (after (nthcdr (1+ posn) directives))
             (string (make-array 10 :element-type 'character
                                    :adjustable t :fill-pointer 0)))
        (unwind-protect
             (with-output-to-string (stream string)
               (setf args (interpret-directive-list stream before orig-args args)))
          (princ (funcall
                  (if colonp
                      (if atsignp 'nstring-upcase 'nstring-capitalize)
                      (if atsignp 'nstring-capitalize-first 'nstring-downcase))
                  string) stream))
        after))))

(def-complex-format-directive #\) ()
  (error 'format-error
         :complaint "No corresponding open paren."))

(def-complex-format-interpreter #\) ()
  (error 'format-error
         :complaint "No corresponding open paren."))


;;;; Conditionals

(defun parse-conditional-directive (directives)
  (let ((sublists nil)
        (last-semi-with-colon-p nil)
        (remaining directives))
    (loop
      (let ((close-or-semi (find-directive remaining #\] t)))
        (unless close-or-semi
          (error 'format-error
                 :complaint "No corresponding close bracket."))
        (let ((posn (position close-or-semi remaining)))
          (push (subseq remaining 0 posn) sublists)
          (setf remaining (nthcdr (1+ posn) remaining))
          (when (char= (format-directive-character close-or-semi) #\])
            (return))
          (setf last-semi-with-colon-p
                (format-directive-colonp close-or-semi)))))
    (values sublists last-semi-with-colon-p remaining)))

(def-complex-format-directive #\[ (colonp atsignp params directives)
  (multiple-value-bind
      (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (values
     (if atsignp
         (if colonp
             (error 'format-error
                    :complaint
                    "Cannot specify both the colon and at-sign modifiers.")
             (if (cdr sublists)
                 (error 'format-error
                        :complaint
                        "Can only specify one section")
                 (expand-bind-defaults () params
                   (expand-maybe-conditional (car sublists)))))
         (if colonp
             (if (= (length sublists) 2)
                 (expand-bind-defaults () params
                   (expand-true-false-conditional (car sublists)
                                                  (cadr sublists)))
                 (error 'format-error
                        :complaint
                        "Must specify exactly two sections."))
             (expand-bind-defaults ((index nil)) params
               (setf *only-simple-args* nil)
               (let* ((clauses nil)
                      (case `(or ,index ,(expand-next-arg))))
                 (when last-semi-with-colon-p
                   (push `(t ,@(expand-directive-list (pop sublists)))
                         clauses))
                 (let ((count (length sublists)))
                   (dolist (sublist sublists)
                     (push `(,(decf count)
                             ,@(expand-directive-list sublist))
                           clauses)))
                 `(case ,case ,@clauses)))))
     remaining)))

(defun expand-maybe-conditional (sublist)
  (flet ((hairy ()
           `(let ((prev-args args)
                  (arg ,(expand-next-arg)))
              (when arg
                (setf args prev-args)
                ,@(expand-directive-list sublist)))))
    (if *only-simple-args*
        (multiple-value-bind (guts new-args)
            (let ((*simple-args* *simple-args*))
              (values (expand-directive-list sublist)
                      *simple-args*))
          (cond ((and new-args (eq *simple-args* (cdr new-args)))
                 (setf *simple-args* new-args)
                 `(when ,(caar new-args)
                    ,@guts))
                (t
                 (setf *only-simple-args* nil)
                 (hairy))))
        (hairy))))

(defun expand-true-false-conditional (true false)
  (let ((arg (expand-next-arg)))
    (flet ((hairy ()
             `(if ,arg
                  (progn
                    ,@(expand-directive-list true))
                  (progn
                    ,@(expand-directive-list false)))))
      (if *only-simple-args*
          (multiple-value-bind
              (true-guts true-args true-simple)
              (let ((*simple-args* *simple-args*)
                    (*only-simple-args* t))
                (values (expand-directive-list true)
                        *simple-args*
                        *only-simple-args*))
            (multiple-value-bind
                (false-guts false-args false-simple)
                (let ((*simple-args* *simple-args*)
                      (*only-simple-args* t))
                  (values (expand-directive-list false)
                          *simple-args*
                          *only-simple-args*))
              (if (= (length true-args) (length false-args))
                  `(if ,arg
                       (progn
                         ,@true-guts)
                       ,(do ((false false-args (cdr false))
                             (true true-args (cdr true))
                             (bindings nil (cons `(,(caar false) ,(caar true))
                                                 bindings)))
                            ((eq true *simple-args*)
                             (setf *simple-args* true-args)
                             (setf *only-simple-args*
                                   (and true-simple false-simple))
                             (if bindings
                                 `(let ,bindings
                                    ,@false-guts)
                                 `(progn
                                    ,@false-guts)))))
                  (progn
                    (setf *only-simple-args* nil)
                    (hairy)))))
          (hairy)))))



(def-complex-format-interpreter #\[ (colonp atsignp params directives)
  (multiple-value-bind
        (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (setf args
          (if atsignp
              (if colonp
                  (error 'format-error
                         :complaint
                         "Cannot specify both the colon and at-sign modifiers.")
                  (if (cdr sublists)
                      (error 'format-error
                             :complaint
                             "Can only specify one section")
                      (interpret-bind-defaults () params
                        (let ((prev-args args)
                              (arg (next-arg)))
                          (if arg
                              (interpret-directive-list stream
                                                        (car sublists)
                                                        orig-args
                                                        prev-args)
                              args)))))
              (if colonp
                  (if (= (length sublists) 2)
                      (interpret-bind-defaults () params
                        (if (next-arg)
                            (interpret-directive-list stream (car sublists)
                                                      orig-args args)
                            (interpret-directive-list stream (cadr sublists)
                                                      orig-args args)))
                      (error 'format-error
                             :complaint
                             "Must specify exactly two sections."))
                  (interpret-bind-defaults ((index (next-arg))) params
                    (let* ((default (and last-semi-with-colon-p
                                         (pop sublists)))
                           (last (1- (length sublists)))
                           (sublist
                             (if (<= 0 index last)
                                 (nth (- last index) sublists)
                                 default)))
                      (interpret-directive-list stream sublist orig-args
                                                args))))))
    remaining))

(def-complex-format-directive #\; ()
  (error 'format-error
         :complaint
         "~~; not contained within either ~~[...~~] or ~~<...~~>."))

(def-complex-format-interpreter #\; ()
  (error 'format-error
         :complaint
         "~~; not contained within either ~~[...~~] or ~~<...~~>."))

(def-complex-format-interpreter #\] ()
  (error 'format-error
         :complaint
         "No corresponding open bracket."))

(def-complex-format-directive #\] ()
  (error 'format-error
         :complaint
         "No corresponding open bracket."))


;;;; Up-and-out.

(defvar *outside-args*)

(def-format-directive #\^ (colonp atsignp params)
  (when atsignp
    (error 'format-error
           :complaint "cannot use the at-sign modifier with this directive"))
  (when (and colonp (not *up-up-and-out-allowed*))
    (error 'format-error
           :complaint "attempt to use ~~:^ outside a ~~:{...~~} construct"))
  `(when ,(expand-bind-defaults ((arg1 nil) (arg2 nil) (arg3 nil)) params
            `(cond (,arg3 (<= ,arg1 ,arg2 ,arg3))
                   (,arg2 (eql ,arg1 ,arg2))
                   (,arg1 (eql ,arg1 0))
                   (t ,(if colonp
                           '(null outside-args)
                           (progn
                             (setf *only-simple-args* nil)
                             '(null args))))))
     ,(if colonp
          '(return-from outside-loop nil)
          '(return))))

(def-format-interpreter #\^ (colonp atsignp params)
  (when atsignp
    (error 'format-error
           :complaint "cannot specify the at-sign modifier"))
  (when (and colonp (not *up-up-and-out-allowed*))
    (error 'format-error
           :complaint "attempt to use ~~:^ outside a ~~:{...~~} construct"))
  (when (interpret-bind-defaults ((arg1 nil) (arg2 nil) (arg3 nil)) params
          (cond (arg3 (<= arg1 arg2 arg3))
                (arg2 (eql arg1 arg2))
                (arg1 (eql arg1 0))
                (t (if colonp
                       (null *outside-args*)
                       (null args)))))
    (throw (if colonp 'up-up-and-out 'up-and-out)
           args)))


;;;; Iteration.

(def-complex-format-directive #\{ (colonp atsignp params string end directives)
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error 'format-error
             :complaint "no corresponding close brace"))
    (let* ((closed-with-colon (format-directive-colonp close))
           (posn (position close directives)))
      (labels
          ((compute-insides ()
             (if (zerop posn)
                 (if *orig-args-available*
                     `((handler-bind
                           ((format-error
                             (lambda (condition)
                               (error 'format-error
                                      :complaint
                                      "~A~%while processing indirect format string:"
                                      :args (list condition)
                                      :print-banner nil
                                      :control-string ,string
                                      :offset ,(1- end)))))
                         (setf args
                               (formatter-aux stream inside-string orig-args args))))
                     (throw 'need-orig-args nil))
                 (let ((*up-up-and-out-allowed* colonp))
                   (expand-directive-list (subseq directives 0 posn)))))
           (compute-loop (count)
             (when atsignp
               (setf *only-simple-args* nil))
             `(loop
                ,@(unless closed-with-colon
                    '((when (null args)
                        (return))))
                ,@(when count
                    `((when (and ,count (minusp (decf ,count)))
                        (return))))
                ,@(if colonp
                      (let ((*expander-next-arg-macro* 'expander-next-arg)
                            (*only-simple-args* nil)
                            (*orig-args-available* t))
                        `((let* ((orig-args ,(expand-next-arg))
                                 (outside-args args)
                                 (args orig-args))
                            (declare (ignorable orig-args outside-args args))
                            (block nil
                              ,@(compute-insides)))))
                      (compute-insides))
                ,@(when closed-with-colon
                    '((when (null args)
                        (return))))))
           (compute-block (count)
             (if colonp
                 `(block outside-loop
                    ,(compute-loop count))
                 (compute-loop count)))
           (compute-bindings (count)
             (if atsignp
                 (compute-block count)
                 `(let* ((orig-args ,(expand-next-arg))
                         (args orig-args))
                   (declare (ignorable orig-args args))
                   ,(let ((*expander-next-arg-macro* 'expander-next-arg)
                          (*only-simple-args* nil)
                          (*orig-args-available* t))
                      (compute-block count))))))
        (values (if params
                    (expand-bind-defaults ((count nil)) params
                      (if (zerop posn)
                          `(let ((inside-string ,(expand-next-arg)))
                            ,(compute-bindings count))
                          (compute-bindings count)))
                    (if (zerop posn)
                        `(let ((inside-string ,(expand-next-arg)))
                          ,(compute-bindings nil))
                        (compute-bindings nil)))
                (nthcdr (1+ posn) directives))))))

(def-complex-format-interpreter #\{
                                (colonp atsignp params string end directives)
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error 'format-error
             :complaint
             "No corresponding close brace."))
    (interpret-bind-defaults ((max-count nil)) params
      (let* ((closed-with-colon (format-directive-colonp close))
             (posn (position close directives))
             (insides (if (zerop posn)
                          (next-arg)
                          (subseq directives 0 posn)))
             (*up-up-and-out-allowed* colonp))
        (labels
            ((do-guts (orig-args args)
               (if (zerop posn)
                   (handler-bind
                       ((format-error
                         #'(lambda (condition)
                             (error 'format-error
                                    :complaint
                            "~A~%while processing indirect format string:"
                                    :arguments (list condition)
                                    :print-banner nil
                                    :control-string string
                                    :offset (1- end)))))
                     (formatter-aux stream insides orig-args args))
                   (interpret-directive-list stream insides
                                             orig-args args)))
             (bind-args (orig-args args)
               (if colonp
                   (let* ((arg (next-arg))
                          (*logical-block-popper* nil)
                          (*outside-args* args))
                     (catch 'up-and-out
                       (do-guts arg arg))
                     args)
                   (do-guts orig-args args)))
             (do-loop (orig-args args)
               (catch (if colonp 'up-up-and-out 'up-and-out)
                 (loop
                   (when (and (not closed-with-colon) (null args))
                     (return))
                   (when (and max-count (minusp (decf max-count)))
                     (return))
                   (setf args (bind-args orig-args args))
                   (when (and closed-with-colon (null args))
                     (return)))
                 args)))
          (if atsignp
              (setf args (do-loop orig-args args))
              (let ((arg (next-arg))
                    (*logical-block-popper* nil))
                (do-loop arg arg)))
          (nthcdr (1+ posn) directives))))))

(def-complex-format-directive #\} ()
  (error 'format-error
         :complaint "No corresponding open brace."))

(def-complex-format-interpreter #\} ()
  (error 'format-error
         :complaint "No corresponding open brace."))

;;;; User-defined method.

(def-format-directive #\/ (string start end colonp atsignp params)
  (let ((symbol (extract-user-function-name string start end)))
    (loop for (_ . param) in params
          for param-name = (gensym)
          collect param-name into param-names
          collect `(,param-name
                    ,(case param
                       (:arg (expand-next-arg))
                       (:remaining '(length args))
                       (t param)))
            into bindings
          finally (return
                    `(let ,bindings
                       (,symbol stream ,(expand-next-arg) ,colonp ,atsignp
                                ,@param-names))))))

(def-format-interpreter #\/ (string start end colonp atsignp params)
  (let ((symbol (extract-user-function-name string start end)))
    (apply (fdefinition symbol) stream (next-arg) colonp atsignp
           (loop for (_ . param) in params
                 collect (case param
                           (:arg (next-arg))
                           (:remaining (length args))
                           (t param))))))

(defun extract-user-function-name (string start end)
  (let ((slash (position #\/ string :start start :end (1- end)
                         :from-end t)))
    (unless slash
      (error 'format-error
             :complaint "Malformed ~~/ directive."))
    (let* ((name (string-upcase (let ((foo string))
                                  ;; Hack alert: This is to keep the compiler
                                  ;; quit about deleting code inside the subseq
                                  ;; expansion.
                                  (subseq foo (1+ slash) (1- end)))))
           (first-colon (position #\: name))
           (second-colon (if first-colon (position #\: name :start (1+ first-colon))))
           (package-name (if first-colon
                             (subseq name 0 first-colon)
                             "COMMON-LISP-USER"))
           (package (find-package package-name)))
      (unless package
        (error 'format-error
               :complaint "No package named ~S"
               :arguments (list package-name)))
      (intern (cond
                ((and second-colon (= second-colon (1+ first-colon)))
                 (subseq name (1+ second-colon)))
                (first-colon
                 (subseq name (1+ first-colon)))
                (t name))
              package))))

;;; Originally contributed by stassats May 24, 2016
#+(or cclasp eclasp)
(define-compiler-macro format (&whole whole destination control-string &rest args
                                      &environment env)
  ;; Be especially nice about the common programmer error of
  ;; (format "control-string" ...)
  (when (and (constantp destination env)
             (stringp (ext:constant-form-value destination env)))
    (ext:with-current-source-form (destination)
      (warn "Literal string as destination in FORMAT:~%  ~s" whole))
    (return-from format whole))
  (let ((original-control-string control-string)
        (control-string (and (constantp control-string env)
                             (ext:constant-form-value control-string env))))
    (if (stringp control-string)
        (let ((dest-sym (gensym "DEST"))
              (stream-sym (gensym "STREAM")))
          (multiple-value-bind (guts variables)
              ;; We call %formatter-guts here because it has the side effect
              ;; of signaling an error if the control string is invalid.
              ;; We want to do that before check-min/max-format-arguments.
              (ext:with-current-source-form (original-control-string)
                (%formatter-guts control-string))
            (check-min/max-format-arguments control-string (length args))
            (let* ((body
                     (if (eq variables 't)
                         `(,(%formatter-lambda control-string guts variables)
                           ,stream-sym ,@args)
                         (gen-inline-format
                          control-string guts variables stream-sym args)))
                   (dest-constantp (constantp destination env))
                   (dest (and dest-constantp
                              (ext:constant-form-value destination env))))
              ;; If the destination is constant T or NIL, avoid bothering with it
              ;; at runtime.
              ;; NOTE: With constant propagation this would be unnecessary.
              (cond ((and dest-constantp (eq dest nil))
                     `(with-output-to-string (,stream-sym) ,body))
                    ((eq dest 't) ; must be constant
                     `(let ((,stream-sym *standard-output*)) ,body))
                    (t
                     ;; no dice - runtime dispatch
                     ;; NOTE: If the body exits abnormally, which it can because of
                     ;; ~// or just argument evaluations, the string stream is not
                     ;; closed. However unlike normal with-output-to-string, nothing
                     ;; can refer to it, so hopefully it'll just be GC'd normally.
                     `(let* ((,dest-sym ,destination)
                             (,stream-sym
                               (cond ((null ,dest-sym)
                                      (make-string-output-stream))
                                     ((eq ,dest-sym t) *standard-output*)
                                     ((stringp ,dest-sym)
                                      (core:make-string-output-stream-from-string
                                       ,dest-sym))
                                     (t ,dest-sym))))
                        ,body
                        (if (null ,dest-sym)
                            (get-output-stream-string ,stream-sym)
                            nil)))))))
        whole)))

;;; Given a formatter form that doesn't do anything fancy with arguments,
;;; expand into some code to execute it with the given args.
;;; NOTE: If we could inline functions with &optional &rest, this would be
;;; redundant. At the moment we can't.
(defun gen-inline-format (control-string guts variables streamvar args)
  (if (> (length variables) (length args))
      ;; not enough args is special cased.
      ;; note check-min/max-format-arguments already issued a warning.
      (let* ((nargs (length args))
             (first-unsupplied-offset (cdr (nth nargs variables)))
             (varsyms (mapcar #'car variables))
             (bound-vars (subseq varsyms 0 nargs)))
        `(let (,@(mapcar #'list bound-vars args)
               (stream ,streamvar))
           (declare (ignore ,@bound-vars stream))
           ,(simple-formatter-param-err-form control-string
                                             first-unsupplied-offset)))
      ;; Normal case
      (let* ((varsyms (mapcar #'car variables)))
        `(let (,@(mapcar #'list varsyms args)
               ;; It's important that we bind these fixed variables
               ;; only AFTER evaluating the format arguments.
               ;; Otherwise, something like (format nil ... core::stream)
               ;; will be problematic.
               (stream ,streamvar)
               ;; Remaining arguments are collected in a list.
               ;; They can be used by e.g. ~@{
               (args (list ,@(nthcdr (length variables) args))))
           (declare (ignorable args))
           ,guts))))

;;;; Compile-time checking of format arguments and control string

;;; Conditions the FORMAT compiler macro signals if there's an argument count mismatch.
;;; CLHS 22.3.10.2 says that having too few arguments is undefined, so that's a warning,
;;; but having too many just means they're ignored, so that's a style-warning.
;;; (Alternately we could not complain at all.)
(define-condition format-warning-too-few-arguments (warning)
  ((control-string :initarg :control :reader format-warning-control-string)
   (expected :initarg :expected :reader format-warning-expected)
   (observed :initarg :observed :reader format-warning-observed))
  (:report (lambda (condition stream)
             (format stream
                     "Format string ~s expects at least ~d arguments,~@
                      but is only provided ~d."
                     (format-warning-control-string condition)
                     (format-warning-expected condition)
                     (format-warning-observed condition)))))
(define-condition format-warning-too-many-arguments (style-warning)
  ((control-string :initarg :control :reader format-warning-control-string)
   (expected :initarg :expected :reader format-warning-expected)
   (observed :initarg :observed :reader format-warning-observed))
  (:report (lambda (condition stream)
             (format stream
                     "Format string ~s expects at most ~d arguments,~@
                      but is provided ~d."
                     (format-warning-control-string condition)
                     (format-warning-expected condition)
                     (format-warning-observed condition)))))

;;;
;;; Signal a warning if the given control string will not work with
;;; the given number of arguments. Assumes the control string's validity.
;;;
(defun check-min/max-format-arguments (control-string nargs)
  (multiple-value-bind (min max)
      (catch 'give-up
        (%min/max-format-args (tokenize-control-string control-string)))
    (cond ((and min (< nargs min))
           (warn 'format-warning-too-few-arguments
                 :control control-string
                 :expected min
                 :observed nargs))
          ((and max (> nargs max))
           (warn 'format-warning-too-many-arguments
                 :control control-string
                 :expected min :observed nargs)))))

(defun %min/max-format-args (directives)
  (let ((min-req 0) (max-req 0))
    (flet ((incf-both (&optional (n 1))
             (incf min-req n)
             (incf max-req n)))
      (loop
        (let ((dir (pop directives)))
          (when (null dir)
            (return (values min-req max-req)))
          (when (format-directive-p dir)
            (incf-both (count :arg (format-directive-params dir) :key #'cdr))
            (let ((c (format-directive-character dir)))
              (cond ((find c "ABCDEFGORSWX$/")
                     (incf-both))
                    ((char= c #\P)
                     (unless (format-directive-colonp dir)
                       (incf-both)))
                    ((or (find c "IT%&|_<>();") (char= c #\newline)))
                    ((char= c #\[)
                     (multiple-value-bind (min max remaining)
                         (%min/max-conditional-args dir directives)
                       (setq directives remaining)
                       (incf min-req min)
                       (incf max-req max)))
                    ((char= c #\{)
                     (multiple-value-bind (min max remaining)
                         (%min/max-iteration-args dir directives)
                       (setq directives remaining)
                       (incf min-req min)
                       (incf max-req max)))
                    ((char= c #\?)
                     (cond ((format-directive-atsignp dir)
                            (incf min-req)
                            (setq max-req most-positive-fixnum))
                           (t (incf-both 2))))
                    (t (throw 'give-up nil))))))))))

;;;
;;; ANSI: if arg is out of range, no clause is selected.  That means
;;; the minimum number of args required for the interior of ~[~] is
;;; always zero.
;;;
(defun %min/max-conditional-args (conditional directives)
  (multiple-value-bind (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (declare (ignore last-semi-with-colon-p))
    (let ((sub-max (loop for s in sublists maximize
                                           (nth-value 1 (%min/max-format-args s))))
          (min-req 1)
          max-req)
      (cond ((format-directive-atsignp conditional)
             (setq max-req (max 1 sub-max)))
            ((loop for p in (format-directive-params conditional)
                     thereis (or (integerp (cdr p))
                                 (member (cdr p) '(:remaining :arg) :test #'eq)))
             (setq min-req 0)
             (setq max-req sub-max))
            (t
             (setq max-req (1+ sub-max))))
      (values min-req max-req remaining))))

(defun %min/max-iteration-args (iteration directives)
  (let* ((close (find-directive directives #\} nil))
         (posn (position close directives))
         (remaining (nthcdr (1+ posn) directives)))
    (if (format-directive-atsignp iteration)
        (values (if (zerop posn) 1 0) most-positive-fixnum remaining)
        (let ((nreq (if (zerop posn) 2 1)))
          (values nreq nreq remaining)))))

(eval-when (:load-toplevel :execute)
  ;; Swap in the cl:format command
  (setf (fdefinition 'cl:format) #'sys::format-std))
