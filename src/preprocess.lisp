(in-package :cl-user)
(defpackage cmacro.preprocess
  (:use :cl))
(in-package :cmacro.preprocess)


(defparameter +cc+ "gcc")

(defparameter +preproc-cmd+
  (format nil "~A -xc - -E"
          +cc+))

(defun preprocess (data)
  (multiple-value-bind (stdout stderr exit-code)
      (trivial-shell:shell-command +preproc-cmd+ :input data)
    (when (not (eql exit-code 0))
      (format t "An error occurred during preprocessing:~&")
      (format t "~A" stderr)
      (sb-ext:quit))
    stdout))

(defparameter +cmc-lexer-bin+ "/usr/bin/cmc-lexer")
