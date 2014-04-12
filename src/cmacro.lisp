(in-package :cl-user)
(defpackage cmacro
  (:use :cl))
(in-package :cmacro)

(defun extract-and-macroexpand (data)
  (destructuring-bind (ast macros) 
      (cmacro.macro:extract-macro-definitions (cmacro.parse:parse-data data))
    (cmacro.macro:macroexpand-ast ast macros)))

(defun macroexpand-data (data)
  (cmacro.parse:print-ast (extract-and-macroexpand data)))

(defun macroexpand-pathname (pathname)
  (macroexpand-data (cmacro.preprocess::slurp-file pathname)))
