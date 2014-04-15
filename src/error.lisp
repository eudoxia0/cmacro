(in-package :cl-user)
(defpackage cmacro.error
  (:use :cl)
  (:import-from :cmacro.parse
                :token-text)
  (:export :bad-macro-definition
           :bad-match
           :no-input-files))
(in-package :cmacro.error)

(setf *debugger-hook* #'(lambda (c h) (format t "~A~&" c) (sb-ext:quit)))

(define-condition cmacro-error () ())

(defparameter +bad-match-msg+
  "Error trying to macroexpand '~A': The input didn't match any cases.")

(define-condition bad-macro-definition (cmacro-error)
  ((text :initarg :text :reader text))

  (:report
   (lambda (condition stream)
     (format stream "Bad macro definition: ~A." (text condition)))))

(define-condition bad-match (cmacro-error)
  ((token :initarg :token :reader token))

  (:report
   (lambda (condition stream)
     (let ((token (token condition)))
       (format stream
               +bad-match-msg+
               (token-text token))))))

(define-condition no-input-files (cmacro-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No input files."))))
