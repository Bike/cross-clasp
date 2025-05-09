(defun sys::load-foreign-libraries ()
  (when (find-package :cffi)
    (loop with list-foreign-libraries = (find-symbol "LIST-FOREIGN-LIBRARIES" :cffi)
          with load-foreign-library = (find-symbol "LOAD-FOREIGN-LIBRARY" :cffi)
          with foreign-library-name = (find-symbol "FOREIGN-LIBRARY-NAME" :cffi)
          for lib in (ignore-errors (funcall list-foreign-libraries :loaded-only nil))
          for name = (ignore-errors (funcall foreign-library-name lib))
          do (ignore-errors (funcall load-foreign-library name)))))

(defun sys::load-extensions ()
  (when (and core:*extension-systems*
             (notany (lambda (feature)
                       (member feature '(:ignore-extensions :ignore-extension-systems)))
                     *features*))
    (require :asdf)
    (loop with load-system = (or (ignore-errors (find-symbol "QUICKLOAD" :quicklisp))
                                 (find-symbol "LOAD-SYSTEM" :asdf))
          for system in core:*extension-systems*
          do (funcall load-system system))))

(defun sys::call-initialize-hooks ()
  (loop for hook in core:*initialize-hooks*
        do (funcall hook)))

(defun sys::call-terminate-hooks ()
  (loop for hook in core:*terminate-hooks*
        do (funcall hook)))

(defun sys::maybe-load-clasprc ()
  "Maybe load the users startup code"
  (unless (core:no-rc-p)
    (let ((clasprc (core:rc-file-name)))
      (if (probe-file clasprc)
          (progn
            (unless (core:noinform-p)
              (format t "Loading resource file ~a~%" clasprc))
            (core:load-source clasprc))
          (unless (core:noinform-p)
            (format t "Resource file ~a not found, skipping loading of it.~%" clasprc))))))

(defun sys::process-command-line-load-eval-sequence ()
  (loop for (cmd . arg) in (core:command-line-load-eval-sequence)
        do (ecase cmd
             (:load (load arg))
             (:script (core:load-source arg nil nil nil t))
             (:eval (eval (read-from-string arg))))))

(defun sys::standard-toplevel ()
  (ext:lock-package "CORE")
  
  #-staging (when (ext:getenv "CLASP_AUTOCOMPILATION")
              (funcall 'ext:start-autocompilation))
  (case (core:startup-type)
    ((:snapshot-file :embedded-snapshot)
     (sys::load-foreign-libraries))
    (otherwise
     (core::maybe-load-clasprc)
     (sys::load-extensions)))
  (sys::call-initialize-hooks)
  (unwind-protect
       (progn
         (core::process-command-line-load-eval-sequence)
         (if (core:is-interactive-lisp)
             (core:top-level)
             (core:exit 0)))
    (sys::call-terminate-hooks)))

(setf ext:*toplevel-hook* 'sys::standard-toplevel)
