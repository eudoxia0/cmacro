(in-package :cl-user)
(defpackage cmacro.error
  (:use :cl)
  (:export :parser-error))
(in-package :cmacro.error)

;(setf *debugger-hook* #'(lambda (c h) (format t "~A~&" c)
;                          (sb-ext:quit :unix-status -1)))

(define-condition cmacro-error () ())

(define-condition parser-error (cmacro-error)
  ((text :initarg :text :reader text :type string)
   (line :initarg :line :reader line :type integer)
   (column :initarg :column :reader column :type integer))

  (:report
   (lambda (condition stream)
     (format stream "~&Parsing error at line ~A, column ~A: ~&~A"
             (line condition)
             (column condition)
             (text condition)))))

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
