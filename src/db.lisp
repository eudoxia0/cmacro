(in-package :cl-user)
(defpackage cmacro.db
  (:use :cl)
  (:export :gen-sym
           :get-sym))
(in-package :cmacro.db)


(defparameter *symbol-index* 0)

(defun make-sym (label n)
  (format nil "cmacro_~A_~A" label n))

(defun gen-sym (label)
  (make-sym label (incf *symbol-index*)))

(defun get-sym (label &optional (n 0))
  (if (< (- *symbol-index* n) 1)
      (make-sym label 0)
      (make-sym label (- *symbol-index* n))))
