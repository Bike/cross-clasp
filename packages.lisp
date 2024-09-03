(defpackage #:cross-clasp.clasp.core
  (:use #:cl)
  (:export #:+type-header-value-map+ #:header-stamp)
  (:export #:vaslist-length #:vaslist-pop)
  (:export #:operator-shadowed-p #:process-declarations)
  (:export #:simple-program-error)
  (:export #:lambda-name)
  (:export #:parse-bytespec)
  (:export #:put-f #:rem-f)
  (:export #:hash-table-pairs #:hash-eql)
  (:export #:fmt)
  (:export #:name-of-class #:instance-ref)
  (:export #:proper-list-p)
  (:export #:single-float-p #:double-float-p)
  (:export #:data-vector-p #:replace-array #:vref
           #:make-simple-vector-t
           #:sbv-bit-and #:sbv-bit-ior #:sbv-bit-xor #:sbv-bit-eqv
           #:sbv-bit-nand #:sbv-bit-nor #:sbv-bit-andc1 #:sbv-bit-andc2
           #:sbv-bit-orc1 #:sbv-bit-orc2 #:sbv-bit-not)
  (:export #:num-op-asin #:num-op-acos #:num-op-asinh #:num-op-acosh
           #:num-op-atanh)
  (:export #:function-name)
  (:export #:unix-get-local-time-zone #:unix-daylight-saving-time)
  (:export #:thread-local-write-to-string-output-stream
           #:get-thread-local-write-to-string-output-stream-string
           #:write-addr)
  (:export #:invoke-internal-debugger))

(defpackage #:cross-clasp.clasp.gctools
  (:use #:cl)
  (:export #:thread-local-unwind-counter #:bytes-allocated))

(defpackage #:cross-clasp.clasp.mp
  (:use #:cl)
  (:export #:get-atomic-expansion #:define-atomic-expander))

(defpackage #:cross-clasp.clasp.clos
  (:use #:cl))

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
  (:export #:with-current-source-form)
  (:export #:parse-define-setf-expander #:setf-expander)
  (:export #:parse-deftype)
  (:export #:parse-macro #:parse-compiler-macro)
  (:export #:array-index)
  (:export #:add-package-local-nickname #:add-implementation-package
           #:lock-package)
  (:export #:*ed-functions*))

(defpackage #:cross-clasp
  (:use #:cl)
  (:local-nicknames (#:m #:maclina.machine)
                    (#:core #:cross-clasp.clasp.core)
                    (#:clos #:cross-clasp.clasp.clos)
                    (#:gc #:cross-clasp.clasp.gctools)
                    (#:ext #:cross-clasp.clasp.ext))
  (:shadow #:proclaim)
  (:export #:client)
  (:export #:fill-environment))
