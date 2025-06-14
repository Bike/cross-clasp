;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  MP.LSP  -- Multiprocessing capabilities.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

#+threads
(in-package "MP")

#+threads
(let ()
(defmacro without-interrupts (&body body)
  "Executes BODY with all deferrable interrupts disabled. Deferrable
interrupts arriving during execution of the BODY take effect after BODY has
been executed.

Deferrable interrupts include most blockable POSIX signals, and
MP:INTERRUPT-THREAD. Does not interfere with garbage collection, and
unlike in many traditional Lisps using userspace threads, in ECL
WITHOUT-INTERRUPTS does not inhibit scheduling of other threads.

Binds ALLOW-WITH-INTERRUPTS, WITH-LOCAL-INTERRUPTS and WITH-RESTORED-INTERRUPTS
as a local macros.

WITH-RESTORED-INTERRUPTS executes the body with interrupts enabled if and only
if the WITHOUT-INTERRUPTS was in an environment in which interrupts were allowed.

ALLOW-WITH-INTERRUPTS allows the WITH-INTERRUPTS to take effect during the
dynamic scope of its body, unless there is an outer WITHOUT-INTERRUPTS without
a corresponding ALLOW-WITH-INTERRUPTS.

WITH-LOCAL-INTERRUPTS executes its body with interrupts enabled provided that
for there is an ALLOW-WITH-INTERRUPTS for every WITHOUT-INTERRUPTS surrounding
the current one. WITH-LOCAL-INTERRUPTS is equivalent to:

  (allow-with-interrupts (with-interrupts ...))

Care must be taken not to let either ALLOW-WITH-INTERRUPTS or
WITH-LOCAL-INTERRUPTS appear in a function that escapes from inside the
WITHOUT-INTERRUPTS in:

  (without-interrupts
    ;; The body of the lambda would be executed with WITH-INTERRUPTS allowed
    ;; regardless of the interrupt policy in effect when it is called.
    (lambda () (allow-with-interrupts ...)))

  (without-interrupts
    ;; The body of the lambda would be executed with interrupts enabled
    ;; regardless of the interrupt policy in effect when it is called.
    (lambda () (with-local-interrupts ...)))
"
  (core::with-unique-names (outer-allow-with-interrupts outer-interrupts-enabled)
    `(multiple-value-prog1
         (macrolet ((allow-with-interrupts (&body allow-forms)
                      (list* 'let
                             (list (list 'core::*allow-with-interrupts*
                                         ',outer-allow-with-interrupts))
                             allow-forms))
                    (with-restored-interrupts (&body with-forms)
                      (list* 'let
                             (list (list 'core::*interrupts-enabled*
                                         ',outer-interrupts-enabled))
                             with-forms))
                    (with-local-interrupts (&body with-forms)
                      (list 'let*
                            (list (list 'core::*allow-with-interrupts*
                                        ',outer-allow-with-interrupts)
                                  (list 'core::*interrupts-enabled*
                                        ',outer-allow-with-interrupts))
                            (list 'when ',outer-allow-with-interrupts
                                  '(core::check-pending-interrupts))
                            (list* 'locally with-forms))))
           (let* ((,outer-interrupts-enabled si:*interrupts-enabled*)
                  (si:*interrupts-enabled* nil)
                  (,outer-allow-with-interrupts si:*allow-with-interrupts*)
                  (si:*allow-with-interrupts* nil))
             (declare (ignorable ,outer-allow-with-interrupts
                                 ,outer-interrupts-enabled))
             ,@body))
       (when si:*interrupts-enabled*
         (si::check-pending-interrupts)))))
)

(defun interruptiblep ()
  "Returns true iff the current process is interruptible (i.e. not in a WITHOUT-INTERRUPTS block)."
  si:*interrupts-enabled*)

#+threads
(let ()
(defmacro with-interrupts (&body body)
  "Executes BODY with deferrable interrupts conditionally enabled. If there
are pending interrupts they take effect prior to executing BODY.

As interrupts are normally allowed WITH-INTERRUPTS only makes sense if there
is an outer WITHOUT-INTERRUPTS with a corresponding ALLOW-WITH-INTERRUPTS:
interrupts are not enabled if any outer WITHOUT-INTERRUPTS is not accompanied
by ALLOW-WITH-INTERRUPTS."
  (core::with-unique-names (allowp enablep)
    ;; We could manage without ENABLEP here, but that would require
    ;; taking extra care not to ever have *ALLOW-WITH-INTERRUPTS* NIL
    ;; and *INTERRUPTS-ENABLED* T -- instead of risking future breakage
    ;; we take the tiny hit here.
    `(let* ((,allowp si:*allow-with-interrupts*)
            (,enablep si:*interrupts-enabled*)
            (si:*interrupts-enabled* (or ,enablep ,allowp)))
       (when (and ,allowp (not ,enablep))
         (si::check-pending-interrupts))
       (locally ,@body))))

(defmacro with-lock ((lock-form &rest options) &body body)
  (declare (ignore options)) ; none yet
  #-threads
  `(progn ,@body)
  #+threads
  (core::with-unique-names (lock)
    `(let ((,lock ,lock-form))
       (unwind-protect
            (progn
              (get-lock ,lock)
              (locally ,@body))
         (giveup-lock ,lock)))))

#+threads
(defmacro with-rwlock ((lock op) &body body)
  "Acquire rwlock for the dynamic scope of BODY for operation OP,
which is executed with the lock held by current thread, and
WITH-RWLOCK returns the values of body.

Valid values of argument OP are :READ or :WRITE
(for reader and writer access accordingly)."
    (assert (member op '(:read :write) :test #'eq))
    (let ((s-lock (gensym)))
      `(let ((,s-lock ,lock))
         (,(if (eq :read op)
               'shared-lock
               'write-lock)
          ,s-lock)
         (unwind-protect
             (progn
               ,@body)
           (,(if (eq :read op)
                 'shared-unlock
                 'write-unlock)
            ,s-lock)))))
)

#+threads
(defun abort-process (&optional datum &rest arguments)
  "Immediately end the current process abnormally.
If PROCESS-JOIN is called on this process thereafter, it will signal an error of type PROCESS-JOIN-ERROR.
If DATUM is provided, it and ARGUMENTS designate a condition of default type SIMPLE-ERROR. This condition will be attached to the PROCESS-JOIN-ERROR."
  (%abort-process
   (if datum
       (core::coerce-to-condition datum arguments 'simple-error 'abort-process)
       nil)))
