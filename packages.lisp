(defpackage #:cross-clasp.clasp.core
  (:use #:cl)
  (:export #:+type-header-value-map+ #:header-stamp
           #:stamps-adjacent-p)
  (:export #:vaslistp #:vaslist-length #:vaslist-pop
           #:list-from-vaslist)
  (:export #:operator-shadowed-p #:process-declarations)
  (:export #:simple-program-error)
  (:export #:lambda-name)
  (:export #:parse-bytespec)
  (:export #:put-f #:rem-f)
  (:export #:hash-table-pairs #:hash-equal)
  (:export #:fmt)
  (:export #:name-of-class #:instance-ref)
  (:export #:proper-list-p)
  (:export #:single-float-p #:double-float-p)
  (:export #:data-vector-p #:replace-array #:vref
           #:make-simple-vector-t
           #:sbv-bit-and #:sbv-bit-ior #:sbv-bit-xor #:sbv-bit-eqv
           #:sbv-bit-nand #:sbv-bit-nor #:sbv-bit-andc1 #:sbv-bit-andc2
           #:sbv-bit-orc1 #:sbv-bit-orc2 #:sbv-bit-not)
  (:export #:num-op-asin #:num-op-acos #:num-op-atan
           #:num-op-asinh #:num-op-acosh #:num-op-atanh)
  (:export #:car-atomic #:rplaca-atomic #:cas-car
           #:cdr-atomic #:rplacd-atomic #:cas-cdr
           #:atomic-symbol-value #:atomic-set-symbol-value #:cas-symbol-value
           #:atomic-symbol-plist #:atomic-set-symbol-plist #:cas-symbol-plist)
  (:export #:function-name #:setf-function-name)
  (:export #:allocate-standard-instance #:allocate-raw-instance
           #:allocate-raw-funcallable-instance
           #:class-stamp-for-instances #:class-new-stamp
           #:instance-sig-set #:instance-stamp
           #:make-rack #:rack-ref #:instance-rack #:instance-class)
  (:export #:gfbytecode-simple-fun/make)
  (:export #:setf-find-class)
  (:export #:cxx-class #:clbind-cxx-class #:derivable-cxx-class)
  (:export #:unix-get-local-time-zone #:unix-daylight-saving-time)
  (:export #:thread-local-write-to-string-output-stream
           #:get-thread-local-write-to-string-output-stream-string
           #:write-addr)
  (:export #:*echo-repl-tpl-read*)
  (:export #:signal-servicing)
  (:export #:noprint-p #:noinform-p)
  (:export #:file-scope #:file-scope-pathname)
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
  (:export #:make-source-pos-info
           #:source-pos-info-lineno #:source-pos-info-column
           #:source-pos-info-file-handle)
  (:export #:sl-boundp #:unbound))

(defpackage #:cross-clasp.clasp.gctools
  (:use #:cl)
  (:export #:thread-local-unwind-counter #:bytes-allocated))

(defpackage #:cross-clasp.clasp.clos
  (:use #:cl)
  (:local-nicknames (#:core #:cross-clasp.clasp.core))
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
           #:process-name #:process-active-p
           #:interrupt-process #:process-suspend #:process-resume)
  (:export #:get-atomic-expansion #:define-atomic-expander))

(defpackage #:cross-clasp.clasp.llvm-sys
  (:use #:cl)
  (:export #:tag-tests))

(defpackage #:cross-clasp.clasp.sequence
  (:use #:cl)
  (:export #:make-sequence-iterator #:with-sequence-iterator #:dosequence))

(defpackage #:cross-clasp.clasp.ext
  (:use #:cl)
  (:export #:byte2 #:byte4 #:byte8 #:byte16 #:byte32 #:byte64
           #:integer2 #:integer4 #:integer8 #:integer16
           #:integer32 #:integer64)
  (:export #:specialp)
  (:export #:check-arguments-type)
  (:export #:ansi-stream)
  (:export #:+process-standard-output+)
  (:export #:constant-form-value)
  (:export #:with-current-source-form)
  (:export #:parse-define-setf-expander #:setf-expander)
  (:export #:parse-deftype)
  (:export #:parse-macro #:parse-compiler-macro)
  (:export #:array-index)
  (:export #:interactive-interrupt)
  (:export #:add-package-local-nickname #:add-implementation-package
           #:lock-package)
  (:export #:*ed-functions*)
  (:export #:*invoke-debugger-hook*)
  (:export #:tpl-frame #:tpl-argument #:tpl-arguments))

(defpackage #:cross-clasp
  (:use #:cl)
  (:local-nicknames (#:m #:maclina.machine)
                    (#:core #:cross-clasp.clasp.core)
                    (#:clos #:cross-clasp.clasp.clos)
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
