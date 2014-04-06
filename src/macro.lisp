;;;; This file provides the core functionality of cmacro. It handles the
;;;; following:
;;;; * Extraction of `macro` forms from the AST
;;;; * Searching the AST for macro calls
;;;; * Pattern matching of macro case clauses on the AST
;;;; * Macro expansion

(in-package :cl-user)
(defpackage cmacro.macro
  (:use :cl)
  (:import-from :cmacro.preprocess
                :+var-identifier+))
(in-package :cmacro.macro)

(defun parse-case (ast)
  "Extract variables from the AST of a case clause."
  )

(defun defcmacro (name cases))

(defun macro-call-p (name, macros)
  """Determine if an identifier is making a call to a macro."""
  (gethash name macros))

(defun macroexpand-ast (ast, macros)
  (loop for expression in ast do
    (if (listp expression)
        ;; Recur
        (macroexpand-ast (rest ast) macros)
        ;; A regular expression, possibly an identifier
        (if (and (eq (tok-type expression) :ident)
                 (macro-call-p (tok-text expression)))
            ;; Expand the macro
            
            ;; Let it go
            expression))))
