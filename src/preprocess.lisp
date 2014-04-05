(in-package :cl-user)
(defpackage cmacro.preprocess
  (:use :cl))
(in-package :cmacro.preprocess)


(defparameter +cc+ "gcc")

(defparameter +cmc-lexer-bin+ "/usr/bin/cmc-lexer")

(defparameter +preproc-cmd+
  (format nil "~A -xc - -E | ~A"
          +cc+ +cmc-lexer-bin+))

(defun preprocess (data)
  (multiple-value-bind (stdout stderr exit-code)
      (trivial-shell:shell-command +preproc-cmd+ :input data)
    (when (not (eql exit-code 0))
      (format t "An error occurred during preprocessing:~&")
      (format t "~A" stderr))
    stdout))
