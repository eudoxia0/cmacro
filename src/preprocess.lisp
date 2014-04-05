(in-package :cl-user)
(defpackage cmacro.preprocess
  (:use :cl))
(in-package :cmacro.preprocess)


(defparameter +cc+ "gcc")

(defparameter +preproc-cmd+
  (format nil "~A -xc - -E"
          +cc+))

(defmacro with-command ((cmd input &key on-error) &rest body)
  `(multiple-value-bind (stdout stderr exit-code)
       (trivial-shell:shell-command ,cmd :input ,input)
     (when (not (eql exit-code 0))
       ,on-error)
     ,@body))

(defun preprocess (data)
  (with-command (+preproc-cmd+
                 data
                 :on-error (progn
                             (format t "An error occurred during preprocessing:~&")
                             (format t "~A" stderr)
                             (sb-ext:quit)))
    stdout))

(defparameter +cmc-lexer-bin+
  (first
   (remove-if #'null
              (mapcar #'probe-file
                      (list #p"/usr/bin/cmc-lexer"
                            (merge-pathnames
                             #p"grammar/cmc-lexer"
                             (asdf:component-pathname (asdf:find-system :cmacro))))))))

(defun lex (data)
  (with-command (+cmc-lexer-bin+
                 data
                 :on-error
                 (progn
                   (format t "An error occurred during lexing:~&")
                   (format t "~A" stderr)
                   (sb-ext:quit)))
    stdout))
