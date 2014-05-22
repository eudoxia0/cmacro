(in-package :cl-user)
(defpackage cmacro.error
  (:use :cl)
  (:export :parser-error
           :bad-match
           :bad-import
           :unknown-var
           :unknown-template-command
           :no-input-files))
(in-package :cmacro.error)

(define-condition cmacro-error () ())

(define-condition parser-error (cmacro-error)
  ((text :initarg :text :reader text :type string)
   (line :initarg :line :reader line :type integer)
   (column :initarg :column :reader column :type integer))

  (:report
   (lambda (condition stream)
     (format stream "~&Parsing error at line ~A, column ~A: ~&~A~&"
             (line condition)
             (column condition)
             (text condition))
     (loop for i from 1 to (1- (column condition)) do
       (write-char #\~ stream))
     (write-char #\^ stream)
     (terpri stream))))

(defparameter +bad-match-msg+
  "Error trying to macroexpand '~A' on line ~A: The input didn't match any cases.")

(define-condition bad-match (cmacro-error)
  ((macro-name :initarg :name :reader macro-name :type string)
   (line :initarg :line :reader line :type integer))

  (:report
   (lambda (condition stream)
     (format stream
             +bad-match-msg+
             (macro-name condition)
             (line condition)))))

(define-condition bad-import (cmacro-error)
  ((line :initarg :line :reader line :type integer))

  (:report
   (lambda (condition stream)
     (format stream "Bad cmacro_import call at line ~A." (line condition)))))

(define-condition unknown-var (cmacro-error)
  ((var-name :initarg :var-name :reader var-name))

  (:report (lambda (condition stream)
             (format stream "Unknown variable: '~A'."
                     (var-name condition)))))


(define-condition unknown-template-command (cmacro-error)
  ((command :initarg :command :reader command))

  (:report (lambda (condition stream)
             (format stream "Unknown template command '~A'." (command condition)))))

(define-condition no-input-files (cmacro-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "No input files."))))
