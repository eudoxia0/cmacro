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
  (aif (member option args :test #'equal)
       (second it)
       nil))

(defun optp (option)
  (and (>= (length option) 1)
       (char= (elt option 0) #\-)))

(defun files (args)
  (remove-if #'null
             (loop for sub-args on args collecting
               (if (optp (first sub-args))
                   ;; Skip
                   (progn
                     (setf sub-args (rest sub-args))
                     nil)
                   ;; It's a file
                   (first sub-args)))))  

(defparameter +help+ 
"Usage: cmc [file]* [option]*

  -o, --output    Path to the output file
  --dump-json     Dump a JSON of the AST
  -l,--lex        Dump the tokens (Without macroexpanding)
  -n,--no-expand  Don't macroexpand, but remove macro definitions")

(defun main (args)
  (print args))

