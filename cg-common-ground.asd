;;;; cg-common-ground.asd

(asdf:defsystem #:cg-common-ground
  :serial t
  :description "Utils that make source code generation in CL so convenient"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:cl-ppcre #:iterate)
  :components ((:file "package")
               (:file "cg-common-ground")))

(defsystem :cg-common-ground-tests
  :description "Tests for CG-COMMON-GROUND."
  :licence "GPL"
  :depends-on (:cg-common-ground :fiveam :cl-interpol)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cg-common-ground))))
  (load-system :cg-common-ground)
  (funcall (intern "RUN-TESTS" :cg-common-ground)))
