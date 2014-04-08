(in-package :cl-user)
(defpackage cmacro.error
  (:use :cl)
  (:import-from :cmacro.parse
                :token-text)
  (:export :bad-match))
(in-package :cmacro.error)

(defparameter +bad-match-msg+
  "Error trying to macroexpand '~A': The input didn't match any cases.")

(define-condition bad-match ()
  ((token :initarg :token :reader token))

  (:report
   (lambda (condition stream)
     (let ((token (token condition)))
       (format stream
               +bad-match-msg+
               (token-text token))))))
