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
                :+var-identifier+)
  (:import-from :cmacro.parse
                :token-type
                :token-text))
(in-package :cmacro.macro)


(defun defcmacro (name cases))

(defun macro-call-p (name, macros)
  """Determine if an identifier is making a call to a macro."""
  (gethash name macros))

(defun macroexpand-ast (ast macros)
  (loop for sub-ast on ast do
    (let ((expression (first sub-ast)))
      (if (listp expression)
          ;; Recur
          (macroexpand-ast (rest expression) macros)
          ;; An ordinary expression, possibly an identifier
          (aif (and (eq (token-type expression) :ident)
                    (macro-call-p (token-text expression) macros))
               ;; Expand the macro
               (aif (macro-match it sub-ast)
                    ;; The macro matches one of the clauses, so we replace the
                    ;; part of `sub-ast` that matched with the macro output
                    ...
                    ;; The macro didn't match. Signal an error.
                    ...)
               ;; Let it go
               expression)))))
