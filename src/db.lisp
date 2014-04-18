;;;; Implements a simple database so macros can store and retrieve data

(in-package :cl-user)
(defpackage cmacro.db
  (:use :cl :anaphora)
  (:export :store
           :retrieve
           :gen-sym
           :get-sym))
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

(defparameter *symbol-index* 0)

(defun make-sym (label n)
  (format nil "cmacro_~A_~A" label n))

(defun gen-sym (label)
  (make-sym label (incf *symbol-index*)))

(defun get-sym (label &optional (n 0))
  (if (< (- *symbol-index* n) 1)
      (make-sym label 0)
      (make-sym label (- *symbol-index* n))))
