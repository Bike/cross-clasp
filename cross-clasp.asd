(asdf:defsystem #:cross-clasp
  :depends-on (:maclina :extrinsicl :extrinsicl/maclina :clostrum-basic)
  :components ((:file "packages")
               (:file "macrology" :depends-on ("packages"))
               (:file "condition-system-macros" :depends-on ("packages"))
               (:file "mp-macros" :depends-on ("macrology" "packages"))
               (:file "environment" :depends-on ("packages"))
               (:file "build" :depends-on ("packages"))))
