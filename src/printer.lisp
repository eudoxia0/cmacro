(in-package :cl-user)
(defpackage :cmacro.printer
  (:use :cl :anaphora)
  (:import-from :cmacro.token
                :<text-token>
                :token-text
                :<void-token>
                :<operator>
                :<variable>
                :<preproc>
                :var-name
                :var-qualifiers)
  (:export :print-ast))
(in-package :cmacro.printer)

(defmethod print-token ((tok <text-token>) stream)
  (write-string (token-text tok) stream))

(defmethod print-token ((tok <preproc>) stream)
  (write-string (token-text tok) stream)
  (write-char #\Newline stream))

(defmethod print-token ((tok <void-token>) stream)
  t)

(defmethod print-token ((var <variable>) stream)
  (format stream "$(~{~A~#[~:; ~]~})"
          (cons (var-name var) (var-qualifiers var))))

(defun print-list (list stream)
  (print-expression (first list) stream)
  (loop for item in (rest list) do
    (unless (eq (type-of item) '<operator>)
      (write-char #\Space stream))
    (print-expression item stream)))

(defparameter +group-separators+
  '((:list  . (#\( . #\)))
    (:array . (#\[ . #\]))
    (:block . (#\{ . #\}))))

(defun print-group (group-type list stream)
  (flet ((block-extra ()
           (when (eq group-type :block)
             (write-char #\Newline stream))))
    (let* ((separators (cdr (assoc group-type +group-separators+)))
           (open (car separators))
           (close (cdr separators)))
      (write-char open stream)
      (block-extra)
      (print-list list stream)
      (block-extra)
      (write-char close stream)
      (block-extra))))

(defun print-expression (ast stream)
  (when ast
      (if (atom ast)
          (print-token ast stream)
          (if (keywordp (first ast))
              (print-group (first ast) (rest ast) stream)
              (print-list ast stream)))))

(defun print-ast (ast)
  (with-output-to-string (stream)
    (print-expression ast stream)))
