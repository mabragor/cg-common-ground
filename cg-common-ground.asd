;;;; cg-common-ground.asd

(asdf:defsystem #:cg-common-ground
  :serial t
  :description "Utils that make source code generation in CL so convenient"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:cl-ppcre #:iterate)
  :components ((:file "package")
               (:file "cg-common-ground")))

