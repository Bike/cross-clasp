(asdf:defsystem #:cross-clasp
  :depends-on (:maclina :closer-mop :extrinsicl :extrinsicl/maclina :anatomicl
                        :ecclesia :clostrum-basic)
  :components ((:file "packages")
               (:file "environment" :depends-on ("packages"))
               (:file "macrology" :depends-on ("packages"))
               (:file "condition-system-macros" :depends-on ("packages"))
               (:file "mp-macros" :depends-on ("macrology" "packages"))
               (:file "mp-atomics" :depends-on ("packages"))
               (:module "clos" :depends-on ("packages")
                :components ((:file "cpl")
                             (:file "classes")
                             (:file "method-combination" :depends-on ("classes"))
                             (:file "discriminate" :depends-on ("method-combination"))
                             (:file "generics" :depends-on ("discriminate"))
                             (:file "dump" :depends-on ("generics"))))
               (:file "defstruct" :depends-on ("clos"))
               (:file "kernel/clos/define-method-combination"
                :depends-on ("packages"))
               (:file "base" :depends-on ("environment" "clos" "defstruct"
                                                        "condition-system-macros"
                                                        "mp-macros" "mp-atomics"
                                                        "packages"))
               (:file "build" :depends-on ("packages"))))
