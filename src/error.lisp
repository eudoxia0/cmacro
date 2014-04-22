(in-package :cl-user)
(defpackage cmacro.error
  (:use :cl)
  (:import-from :cmacro.parse
                :token-text)
  (:export :bad-macro-definition
           :bad-match
           :no-input-files
           :unknown-template-command))
(in-package :cmacro.error)

(setf *debugger-hook* #'(lambda (c h) (format t "~A~&" c)
                          (sb-ext:quit :unix-status -1)))

(define-condition cmacro-error () ())

(defparameter +bad-match-msg+
  "Error trying to macroexpand '~A' on line ~A: The input didn't match any cases.")

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
               (token-text token)
               (token-line token))))))

(define-condition no-input-files (cmacro-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No input files."))))

(define-condition unknown-template-command (cmacro-error)
  ((command :initarg :command :reader command))

  (:report (lambda (condition stream)
             (format stream "Unknown template command '~A'." (command condition)))))
