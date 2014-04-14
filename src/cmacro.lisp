(in-package :cl-user)
(defpackage cmacro
  (:use :cl :anaphora)
  (:export :main))
(in-package :cmacro)

(defun extract-and-macroexpand (data)
  (destructuring-bind (ast macros) 
      (cmacro.macro:extract-macro-definitions (cmacro.parse:parse-data data))
    (cmacro.macro:macroexpand-ast ast macros)))

(defun macroexpand-data (data)
  (cmacro.parse:print-ast (extract-and-macroexpand data)))

(defun macroexpand-pathname (pathname)
  (macroexpand-data (cmacro.preprocess::slurp-file pathname)))

(defun get-opt (option args)
  (first (member option args :test #'equal)))

(defparameter +help+ 
"Usage: cmc [file]* [option]*

  -o, --output    Path to the output file
  --dump-json     Dump a JSON of the AST
  -l,--lex        Dump the tokens (Without macroexpanding)
  -n,--no-expand  Don't macroexpand, but remove macro definitions")

(defun main (args)
  )
