(defpackage #:option-9
  (:use #:cl)
  (:export #:option-9))

(in-package #:option-9)
#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defparameter *game* nil)
(defparameter *assets* nil)
(defparameter *id* nil)
(defun new-id ()
  (let ((id *id*))
    (incf *id*)
    id))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-accessor-symbol (prefix-symbol &rest args)
    (intern (format nil "~:@{~{~A~}~)" (cons prefix-symbol args))
	    (symbol-package prefix-symbol))))
(defmacro with-type (type expr)
  `(the ,type ,(if (atom expr)
		   expr
		   (expand-call type (binarize expr)))))

(defun expand-call (type expr)
  `(,(car expr) ,@(mapcar #'(lambda (a)
			      `(with-type ,type ,a))
			  (cdr expr))))
