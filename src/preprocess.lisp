(in-package :cl-user)
(defpackage cmacro.preprocess
  (:use :cl))
(in-package :cmacro.preprocess)


(defparameter +cc+ "gcc")

(defparameter +preproc-cmd+
  (format nil "~A -xc - -E"
          +cc+))

(defmacro with-command (cmd input &key on-error &rest body)
  `(multiple-value-bind (stdout stderr exit-code)
       (trivial-shell:shell-command ,cmd :input ,data)
     (when (not (eql exit-code 0))
       ,on-error)
     ,@body))

(defun preprocess (data)
  (with-command 
    +preproc-cmd+
    data
    :on-error (progn
                (format t "An error occurred during preprocessing:~&")
                (format t "~A" stderr)
                (sb-ext:quit))
    stdout))

(defparameter +cmc-lexer-bin+ "/usr/bin/cmc-lexer")

(defun lex (data)
  (with-command 
    +cmc-lexer-bin+
    data
    (progn
      (format t "An error occurred during lexing:~&")
      (format t "~A" stderr)
      (sb-ext:quit))
    stdout))
