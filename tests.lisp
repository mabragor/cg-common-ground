
(in-package :cl-user)

(defpackage :cg-common-ground-tests
  (:use :cl :cg-common-ground :fiveam :iterate)
  (:export #:run-tests))

(in-package :cg-common-ground-tests)

(cl-interpol:enable-interpol-syntax)

(def-suite cg-common-ground)
(in-suite cg-common-ground)

(defun run-tests ()
  (let ((results (run 'cg-common-ground)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test basic
  (is (eq t t)))