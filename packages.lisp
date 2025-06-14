(defpackage #:cross-clasp.clasp.core
  (:use #:cl)
  (:export #:+type-header-value-map+ #:header-stamp
           #:stamps-adjacent-p)
  (:export #:vaslistp #:vaslist-length #:vaslist-pop
           #:list-from-vaslist)
  (:export #:operator-shadowed-p #:process-declarations)
  (:export #:simple-program-error
           #:out-of-extent-unwind #:no-catch-tag
           #:simple-stream-error #:closed-stream
           #:simple-file-error #:file-does-not-exist #:file-exists
           #:simple-package-error #:import-name-conflict #:export-name-conflict
           #:use-package-name-conflict #:unintern-name-conflict
           #:package-lock-violation
           #:do-not-funcall-special-operator #:wrong-number-of-arguments
           #:odd-keywords #:unrecognized-keyword-argument-error
           #:simple-parse-error #:simple-reader-error)
  (:export #:check-pending-interrupts #:terminal-interrupt
           #:signal-code-alist)
  ;; Clasp usually only defines these if the underlying OS has the given signal.
  ;; Defining them unconditionally shouldn't be a problem, though. They'll just
  ;; never actually be signaled.
  (:export #:sigabrt #:sigalrm #:sigbus #:sigchld #:sigcont #:sigemt #:sigfpe
           #:sighup #:sigill #:sigint #:sigio #:sigkill #:sigpipe #:sigpoll
           #:sigprof #:sigpwr #:sigquit #:sigsegv #:sigstop #:sigtstp #:sigsys
           #:sigterm #:sigtrap #:sigttin #:sigttou #:sigurg #:sigusr1 #:sigusr2
           #:sigvtalrm #:sigxcpu #:sigxfsz #:sigwinch)
  (:export #:lambda-name #:lambda-list)
  (:export #:general #:generalp)
  (:export #:parse-bytespec)
  (:export #:put-f #:rem-f)
  (:export #:hash-table-pairs #:hash-equal)
  (:export #:fmt)
  (:export #:name-of-class #:class-source-location #:cxx-method-source-location
           #:instancep #:instance-ref)
  (:export #:proper-list-p)
  (:export #:ratiop
           #:short-float-p #:single-float-p #:double-float-p #:long-float-p)
  (:export #:data-vector-p #:replace-array #:vref
           #:make-simple-vector-t
           #:abstract-simple-vector #:complex-vector #:mdarray #:simple-mdarray
           #:str8ns #:simple-character-string #:str-wns #:bit-vector-ns
           #:sbv-bit-and #:sbv-bit-ior #:sbv-bit-xor #:sbv-bit-eqv
           #:sbv-bit-nand #:sbv-bit-nor #:sbv-bit-andc1 #:sbv-bit-andc2
           #:sbv-bit-orc1 #:sbv-bit-orc2 #:sbv-bit-not)
  (:export #:num-op-asin #:num-op-acos #:num-op-atan
           #:num-op-asinh #:num-op-acosh #:num-op-atanh)
  (:export #:car-atomic #:rplaca-atomic #:cas-car
           #:cdr-atomic #:rplacd-atomic #:cas-cdr
           #:atomic-symbol-value #:atomic-set-symbol-value #:cas-symbol-value
           #:atomic-symbol-plist #:atomic-set-symbol-plist #:cas-symbol-plist)
  (:export #:function-name #:setf-function-name #:setf-lambda-list
           #:function-docstring #:function-source-pos #:set-source-pos-info)
  (:export #:single-dispatch-generic-function-p)
  (:export #:compute-instance-creator
           #:allocate-standard-instance #:allocate-raw-instance
           #:allocate-raw-funcallable-instance
           #:class-stamp-for-instances #:class-new-stamp
           #:instance-sig #:instance-sig-set #:instance-stamp
           #:make-rack #:rack-sig #:rack-ref #:instance-rack #:instance-class)
  (:export #:gfbytecode-simple-fun/make #:bytecode-simple-fun
           #:function/entry-point
           #:make-closure #:closure-length #:closure-ref)
  (:export #:setf-find-class)
  (:export #:cxx-object-p #:cxx-class #:clbind-cxx-class #:derivable-cxx-class)
  (:export #:unix-get-local-time-zone #:unix-daylight-saving-time)
  (:export #:thread-local-write-to-string-output-stream
           #:get-thread-local-write-to-string-output-stream-string
           #:write-addr)
  (:export #:*echo-repl-tpl-read*)
  (:export #:signal-servicing)
  (:export #:noprint-p #:noinform-p)
  (:export #:quasiquote #:*quasiquote*
           #:unquote #:unquote-nsplice #:unquote-splice)
  (:export #:file-scope #:file-scope-pathname #:mkstemp)
  (:export #:interpret)
  (:export #:wrong-number-of-arguments #:sequence-out-of-bounds)
  (:export #:set-breakstep #:unset-breakstep #:breakstepping-p
           #:invoke-internal-debugger #:debugger-disabled-p)
  (:export #:call-with-frame #:primitive-print-backtrace
           #:debugger-frame-up #:debugger-frame-down
           #:debugger-frame-fname #:debugger-frame-source-position
           #:debugger-frame-function-description #:debugger-frame-lang
           #:debugger-frame-closure #:debugger-frame-xep-p
           #:debugger-frame-args-available-p #:debugger-frame-args
           #:debugger-frame-locals)
  (:export #:function-description-lambda-list
           #:function-description-source-pathname
           #:function-description-lineno #:function-description-column
           #:function-description-docstring)
  (:export #:package-documentation
           #:package-local-nicknames-internal
           #:call-with-package-read-lock #:call-with-package-read-write-lock)
  (:export #:*variable-source-infos*)
  (:export #:make-source-pos-info
           #:source-pos-info-lineno #:source-pos-info-column
           #:source-pos-info-file-handle #:source-pos-info-filepos)
  (:export #:load-source)
  (:export #:command-line-load-eval-sequence
           #:rc-file-name #:no-rc-p #:noinform-p
           #:is-interactive-lisp #:load-extensions #:startup-type
           #:*extension-systems* #:*initialize-hooks* #:*terminate-hooks*)
  (:export #:*use-interpreter-for-eval*)
  (:export #:sl-boundp #:unbound)
  (:export #:quit))

(defpackage #:cross-clasp.clasp.gctools
  (:use #:cl)
  (:export #:thread-local-unwind-counter #:bytes-allocated))

(defpackage #:cross-clasp.clasp.clos
  (:use #:cl)
  (:local-nicknames (#:core #:cross-clasp.clasp.core)
                    (#:mop #:closer-mop))
  (:shadow #:define-method-combination)
  (:export #:slot-value-using-class)
  (:export #:standard-instance-access
           #:funcallable-standard-instance-access)
  (:export #:set-funcallable-instance-function))

(defpackage #:cross-clasp.clasp.mp
  (:use #:cl)
  (:local-nicknames (#:core #:cross-clasp.clasp.core)
                    (#:clos #:cross-clasp.clasp.clos))
  (:export #:make-lock #:get-lock #:giveup-lock)
  (:export #:make-shared-mutex
           #:shared-lock #:write-lock
           #:shared-unlock #:write-unlock)
  (:export #:make-condition-variable
           #:condition-variable-wait #:condition-variable-signal)
  (:export #:with-lock #:with-rwlock
           #:without-interrupts #:with-interrupts)
  (:export #:*current-process* #:all-processes
           #:process #:process-name #:process-active-p
           #:interrupt-process #:process-suspend #:process-resume
           #:suspend-loop #:abort-process #:process-kill #:process-cancel)
  (:export #:process-error #:process-error-process
           #:process-join-error #:process-join-error-original-condition
           #:process-join-error-aborted
           #:push-default-special-binding)
  (:export #:interrupt #:service-interrupt #:enqueue-interrupt
           #:signal-pending-interrupts #:signal-interrupt #:raise
           #:interactive-interrupt #:simple-interrupt #:simple-interactive-interrupt
           #:cancellation-interrupt #:call-interrupt #:call-interrupt-function
           #:suspension-interrupt #:posix-interrupt)
  (:export #:get-atomic-expansion #:define-atomic-expander
           #:not-atomic #:not-atomic-place))

(defpackage #:cross-clasp.clasp.llvm-sys
  (:use #:cl)
  (:export #:tag-tests))

(defpackage #:cross-clasp.clasp.cmp
  (:use #:cl)
  (:export #:code-walk)
  (:export #:*btb-compile-hook* #:*cleavir-compile-hook*))

(defpackage #:cross-clasp.clasp.sequence
  (:use)
  (:export #:make-sequence-iterator #:with-sequence-iterator #:dosequence)
  (:export #:elt #:length #:make-sequence-like #:adjust-sequence)
  (:export #:make-simple-sequence-iterator
           #:iterator-step #:iterator-endp #:iterator-element
           #:iterator-index #:iterator-copy)
  (:export #:protocol-unimplemented #:protocol-unimplemented-operation)
  (:export #:emptyp #:count #:count-if #:count-if-not #:find #:find-if #:find-if-not
           #:position #:position-if #:position-if-not #:subseq #:copy-seq #:fill
           #:nsubstitute #:nsubstitute-if #:nsubstitute-if-not
           #:substitute #:substitute-if #:substitute-if-not
           #:replace #:nreverse #:reverse #:reduce #:mismatch #:search
           #:delete #:delete-if #:delete-if-not #:remove #:remove-if #:remove-if-not
           #:delete-duplicates #:remove-duplicates #:sort #:stable-sort)
  (:export #:make-sequence #:define-iterative-sequence
           #:define-random-access-sequence #:make-random-access-iterator))

(defpackage #:cross-clasp.clasp.debug
  (:use #:cl)
  (:export #:with-truncated-stack)
  (:export #:step-form #:step-into #:step-over)
  (:export #:frame-arguments))

(defpackage #:cross-clasp.clasp.ext
  (:use #:cl)
  (:export #:byte2 #:byte4 #:byte8 #:byte16 #:byte32 #:byte64
           #:integer2 #:integer4 #:integer8 #:integer16
           #:integer32 #:integer64)
  (:export #:specialp #:symbol-macro)
  (:export #:check-arguments-type)
  (:export #:ansi-stream)
  (:export #:+process-standard-input+ #:+process-standard-output+
           #:+process-error-output+ #:+process-terminal-io+)
  (:export #:constant-form-value)
  (:export #:with-current-source-form #:current-source-location
           #:source-location #:source-location-pathname)
  (:export #:function-lambda-list)
  (:export #:type-expander)
  (:export #:parse-define-setf-expander #:setf-expander)
  (:export #:parse-deftype)
  (:export #:parse-macro #:parse-compiler-macro)
  (:export #:array-index)
  (:export #:interactive-interrupt)
  (:export #:add-package-local-nickname #:add-implementation-package
           #:package-implemented-by-list
           #:lock-package #:unlock-package #:package-locked-p)
  (:export #:*ed-functions*)
  (:export #:*invoke-debugger-hook* #:*inspector-hook*
           #:restart-associated-conditions
           #:restart-function #:restart-report-function
           #:restart-interactive-function #:restart-test-function)
  (:export #:segmentation-violation
           #:interactive-interrupt
           #:stack-overflow #:stack-overflow-size #:stack-overflow-type
           #:storage-exhausted #:bus-error
           #:name-conflict #:name-conflict-candidates #:resolve-conflict
           #:undefined-class #:assert-error
           #:character-coding-error #:encoding-error #:decoding-error
           #:character-encoding-error #:character-decoding-error
           #:stream-encoding-error #:stream-decoding-error)
  (:export #:tpl-frame #:tpl-argument #:tpl-arguments)
  (:export #:ansi-stream)
  (:export #:annotate #:*module-provider-functions*)
  (:export #:getenv)
  (:export #:*toplevel-hook*)
  (:export #:compiler-note #:start-autocompilation)
  (:import-from #:cross-clasp.clasp.core #:quit)
  (:export #:quit))

(defpackage #:cross-clasp.clasp.gray
  (:use #:cl)
  (:shadow #:streamp #:open-stream-p #:input-stream-p #:output-stream-p)
  (:shadow #:pathname #:truename)
  (:shadow #:stream-external-format #:stream-element-type)
  (:shadow #:close)
  (:import-from #:cross-clasp.clasp.ext #:ansi-stream)
  (:export #:fundamental-stream
           #:fundamental-input-stream #:fundamental-output-stream
           #:fundamental-character-stream #:fundamental-binary-stream
           #:fundamental-character-input-stream #:fundamental-character-output-stream
           #:fundamental-binary-input-stream #:fundamental-binary-output-stream)
  (:export #:streamp #:input-stream-p #:output-stream-p
           #:open-stream-p #:stream-interactive-p)
  (:export #:stream-write-sequence #:stream-read-sequence)
  (:export #:stream-write-char #:stream-unread-char
           #:stream-peek-char #:stream-read-char
           #:stream-write-string #:stream-read-line
           #:stream-read-char-no-hang #:stream-terpri #:stream-fresh-line)
  (:export #:stream-write-byte #:stream-read-byte)
  (:export #:stream-clear-input #:stream-clear-output #:stream-listen
           #:stream-finish-output #:stream-force-output)
  (:export #:stream-element-type #:stream-external-format
           #:stream-file-length #:stream-file-string-length)
  (:export #:pathname #:truename #:stream-file-descriptor)
  (:export #:close)
  (:export #:stream-input-line #:stream-input-column
           #:stream-line-number #:stream-start-line-p
           #:stream-line-length #:stream-line-column
           #:stream-file-position #:stream-advance-to-column))

(defpackage #:cross-clasp
  (:use #:cl)
  (:local-nicknames (#:m #:maclina.machine)
                    (#:core #:cross-clasp.clasp.core)
                    (#:clos #:cross-clasp.clasp.clos)
                    (#:cmp #:cross-clasp.clasp.cmp)
                    (#:gray #:cross-clasp.clasp.gray)
                    (#:gc #:cross-clasp.clasp.gctools)
                    (#:mp #:cross-clasp.clasp.mp)
                    (#:llvm-sys #:cross-clasp.clasp.llvm-sys)
                    (#:ext #:cross-clasp.clasp.ext))
  (:shadow #:proclaim #:constantp)
  (:export #:client)
  (:export #:fill-environment)
  (:export #:find-compiler-class #:gf-info)
  (:export #:build-macroexpand #:build-macroexpand-1
           #:describe-variable
           #:constantp #:constant-form-value )
  (:export #:build))
