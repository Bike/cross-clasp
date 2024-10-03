(asdf:defsystem #:cross-clasp
  :depends-on (:maclina :extrinsicl :extrinsicl/maclina :clostrum-basic)
  :components ((:file "packages")
               (:file "macrology" :depends-on ("packages"))
               (:file "condition-system-macros" :depends-on ("packages"))
               (:file "mp-macros" :depends-on ("macrology" "packages"))
               (:file "clos-cpl" :depends-on ("packages"))
               (:file "clos-classes" :depends-on ("packages"))
               (:file "clos-method-combination"
                :depends-on ("clos-classes" "packages"))
               (:file "clos-discriminate"
                :depends-on ("clos-method-combination" "packages"))
               (:file "clos-generics"
                :depends-on ("clos-discriminate" "packages"))
               (:file "environment" :depends-on ("packages"))
               (:file "base" :depends-on ("environment" "packages"))
               (:file "clos-dump"
                :depends-on ("clos-generics" "environment" "packages"))
               (:file "build" :depends-on ("packages"))))
