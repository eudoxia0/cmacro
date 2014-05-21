(in-package :cl-user)
(defpackage :cmacro.printer
  (:use :cl :anaphora)
  (:export :print-ast))
(in-package :cmacro.printer)

(defun print-ast (ast stream)
  (format nil "~A" ast))
