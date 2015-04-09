
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

(test destringify-symbol
  (macrolet ((frob (x y)
	       `(is (string= ,x (string (destringify-symbol ,y))))))
    (frob "+%-F-O-O.-BA-R" "%FOO.BaR")
    (frob "*%FOO.B-AR" "%FOO.B_AR")
    (frob "+%-FOO.-BAR" "%Foo.Bar")
    (frob "%FOO.BAR-BAZ" "%foo.bar_baz")))