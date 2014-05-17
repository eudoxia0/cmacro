(in-package :cl-user)
(defpackage :cmacro.tokens
  (:use :cl)
  (:import-from :trivial-types
                :proper-list)
  (:export :<token>
           :token-line
           :token-column
           :token-text
           :<number>
           :<integer>
           :<real>
           :<string>
           :<operator>
           :<variable>
           :var-name
           :var-qualifiers))
(in-package :cmacro.tokens)

;;; Tokens

(defclass <token> ()
  ((line :initarg :line
         :reader token-line
         :type integer)
   (column :initarg :column
           :reader token-column
           :type integer)))

(defclass <text-token> (<token>)
  ((text :initarg :text
         :reader token-text
         :type string)))

(defmethod print-object ((tok <text-token>) stream)
  (format stream "~A" (token-text tok)))

(defmethod token-equal ((a <text-token>) (b <token>))
  (equal (token-text a) (token-text b)))

(defun ast-equal (ast-a ast-b)
  (let* ((ast-a (flatten ast-a))
         (ast-b (flatten ast-b))
         (len-a (length ast-a))
         (len-b (length ast-b)))
    (when (eql len-a len-b)
      ;; Compare individual items
      (loop for i from 0 to (1- len-a) do
        (if (not (token-equal (nth i ast-a)
                              (nth i ast-b)))
            (return nil)))
        t)))

;;; Token subclasses

(defclass <number> (<text-token>) ())
(defclass <integer> (<number>) ())
(defclass <real> (<number>) ())
(defclass <identifier> (<text-token>) ())
(defclass <string> (<text-token>) ())
(defclass <operator> (<text-token>) ())

;;; Variables

(defclass <variable> (<token>)
  ((name :initarg :name
         :reader var-name
         :type string)
   (qualifiers :initarg :qualifiers
               :reader var-qualifiers
               :type (proper-list string))))
