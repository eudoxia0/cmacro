(in-package :cl-user)
(defpackage :cmacro.macroexpand
  (:use :cl :anaphora)
  (:import-from :cmacro.token
                :<token>
                :<identifier>
                :token-text
                :token-line)
  (:import-from :cmacro.macro
                :case-template
                :case-toplevel-template)
  (:import-from :cmacro.pattern
                :match-macro
                :<match>
                :match-length
                :match-bindings
                :match-macro-case)
  (:import-from :cmacro.template
                :render-template)
  (:import-from :cmacro.parser
                :parse-string
                :parse-pathname
                :extract-macros
                :<result>
                :result-ast
                :result-macros)
  (:export :macroexpand-string
           :macroexpand-pathname))
(in-package :cmacro.macroexpand)

(defparameter *found* nil
  "Was a macro found during the last macroexpansion?")
(defparameter *toplevel-expansions* (list))

(defun macro-call-p (token macros)
  (and (subtypep (type-of token) '<identifier>)
       (gethash (token-text token) macros)))

(defmethod expand ((match <match>))
  (let* ((bindings (match-bindings match))
         (macro-case (match-macro-case match))
         (toplevel-ast (aif (case-toplevel-template macro-case)
                            (render-template it bindings)))
         (new-ast  (render-template (case-template macro-case)
                                    bindings)))
    (aif toplevel-ast
         (setf *toplevel-expansions*
               (append *toplevel-expansions* it)))
    new-ast))

(defmethod macroexpand-ast% (ast macros)
  (loop for sub-ast on ast collecting
    (let ((node (first sub-ast)))
      (if (listp node)
          ;; Recur
          (macroexpand-ast% node macros)
          ;; An atom. Is it a macro name?
          (aif (macro-call-p node macros)
               ;; Expand the macro
               (aif (match-macro it (rest sub-ast))
                    ;; The macro matches one of the clauses, so we replace the
                    ;; part of `sub-ast` that matched with the macro output
                    ;; `match-macro` produces a <match> object, see pattern.lisp
                    (progn
                      (setf *found* t)
                      ;; Erase the length of the match from the AST
                      (setf sub-ast (nthcdr (match-length it) sub-ast))
                      ;; Replace it with the macroexpansion
                      (expand it))
                    ;; The macro didn't match. Signal an error.
                    (error 'cmacro.error:bad-match
                           :name (token-text node)
                           :line (token-line node)))
               ;; Not a macro, let it go
               node)))))

(defun macroexpand-ast (ast macros)
  (let ((ast (macroexpand-ast% ast macros)))
    (loop while *found* do
      (setf *found* nil)
      ;; Insert toplevel expansions
      (setf ast (append *toplevel-expansions* ast))
      (setf *toplevel-expansions* (list))
      (setf ast (macroexpand-ast% ast macros)))
    ast))

(defmethod macroexpand-result ((result <result>))
  (macroexpand-ast (result-ast result) (result-macros result)))

(defun macroexpand-string (str)
  (macroexpand-result (extract-macros (parse-string str))))

(defun macroexpand-pathname (pathname)
  (macroexpand-result (extract-macros (parse-pathname pathname))))
