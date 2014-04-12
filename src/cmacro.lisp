(in-package :cl-user)
(defpackage cmacro
  (:use :cl))
(in-package :cmacro)

(defun macroexpand-data (data)
  (destructuring-bind (ast macros) 
      (cmacro.macro:extract-macro-definitions (cmacro.parse:parse-data data))
    (cmacro.parse:print-ast (cmacro.macro:macroexpand-ast ast macros))))

(defun macroexpand-pathname (pathname)
  (macroexpand-data (cmacro.preprocess::slurp-file pathname)))
