(in-package :cl-user)
(defpackage cmacro.macro
  (:use :cl :anaphora)
  (:import-from :trivial-types
                :proper-list)
  (:export :<macro-case>
           :case-match
           :case-template
           :case-toplevel-template
           :<macro>
           :macro-name
           :macro-cases))
(in-package :cmacro.macro)

(defclass <macro-case> ()
  ((match :reader case-match
          :initarg :match)
   (template :reader case-template
             :initarg :template)
   (toplevel-template :reader case-toplevel-template
                      :initarg :toplevel-template)))

(defclass <macro> ()
  ((name :reader macro-name
         :initarg :name
         :type string)
   (cases :reader macro-cases
          :initarg :cases
          :type (proper-list <macro-case))))
