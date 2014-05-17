;;;; This file provides the core functionality of cmacro. It handles the
;;;; following:
;;;; * Extraction of `macro` forms from the AST
;;;; * Searching the AST for macro calls
;;;; * Macro expansion

(in-package :cl-user)
(defpackage cmacro.macro
  (:use :cl :anaphora)
  (:import-from :trivial-types
                :proper-list)
  (:export :<macro-case>
           :<macro>))
(in-package :cmacro.macro)

(defclass <macro-case> ()
  ((match :reader case-match
          :initarg :match)
   (template :reader case-template
             :initarg :template)
   (toplevel-template :reader case-toplevel-template
                      :initarg :toplevel-template)))

(defclass <macro> ()
  ((cases :reader macro-cases
          :initarg :cases
          :type (proper-list <macro-case))))
