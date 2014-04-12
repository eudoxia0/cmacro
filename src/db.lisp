;;;; Implements a simple database so macros can store and retrieve data

(in-package :cl-user)
(defpackage cmacro.db
  (:use :cl :anaphora)
  (:export :store
           :retrieve))
(in-package :cmacro.db)

(defparameter *db* (make-hash-table :test #'equal))

(defun store (key data)
  (setf (gethash key *db*) data))

(defun retrieve (key default)
  (gethash key *db* default))

(defun increase (key val)
  (aif (gethash key *db*)
       (setf (gethash key *db*) (+ val it))
       (setf (gethash key *db*) val)))

(defun decrease (key val)
  (aif (gethash key *db*)
       (setf (gethash key *db*) (- val it))
       (setf (gethash key *db*) (- 0 val))))
