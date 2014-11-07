;;;; cg-common-ground.lisp

(in-package #:cg-common-ground)

(defun joinl (joinee lst)
  (format nil (concatenate 'string "~{~a~^" joinee "~}") lst))
(defun join (joinee &rest lst)
  (joinl joinee lst))

(defun frnl (format-str &rest args)
  (apply #'format `(nil ,format-str ,@args)))
(defun fart (format-str &rest args)
  (apply #'format `(t ,format-str ,@args)))

(defun camelcaseize (name)
  (format nil "~{~a~}" (mapcar #'string-capitalize (cl-ppcre:split "-" (string-downcase name)))))
(defun underscorize (name)
  (format nil "~{~a~^_~}" (cl-ppcre:split "-" (string-downcase name))))
(defun capitalize (name)
  (format nil "~{~a~^_~}" (cl-ppcre:split "-" (string-upcase name))))

(defun camcase-if-not-string (name)
  (if (stringp name)
      name
      (camelcaseize name)))

(defun unscore-if-not-string (name)
  (if (stringp name)
      name
      (underscorize name)))

(defvar *symbol-stringification-style* :unscore)

(defun stringify-symbol (sym)
  (let ((str (string sym)))
    (macrolet ((frob (char)
		 `(if (char= ,char (char str (1- (length str))))
		      (subseq str 1 (1- (length str)))
		      (subseq str 1))))
      (if (equal 0 (length str))
	  str
	  (cond ((char= #\- (char str 0)) (underscorize (frob #\-)))
		((char= #\+ (char str 0)) (camelcaseize (frob #\+)))
		((char= #\* (char str 0)) (capitalize (frob #\*)))
		(t (cond ((eq :unscore *symbol-stringification-style*)
			  (underscorize str))
			 ((eq :camcase *symbol-stringification-style*)
			  (camelcaseize str))
			 ((eq :capital *symbol-stringification-style*)
			  (capitalize str)))))))))
			 
(defun stringify-if-symbol (sym)
  (if (symbolp sym)
      (stringify-symbol sym)
      sym))

(defun parse-out-keywords (kwd-lst lambda-list)
  "Return list (KWD1 KWD2 ... KWDn . OTHER-ARGS) collecting all keyword-looking pairs of arguments in lambda list"
  (let ((kwds (make-array (length kwd-lst) :initial-element nil)))
    (iter (generate elt in lambda-list)
	  (if (keywordp (next elt))
	      (setf (elt kwds (position elt kwd-lst :test #'eq)) (next elt))
	      (collect elt into res))
	  (finally (return (nconc (iter (for kwd in-vector kwds)
					(collect kwd))
				  res))))))


