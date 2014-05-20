(in-package :cl-user)
(defpackage :cmacro.macroexpand
  (:use :cl :anaphora)
  (:import-from :cmacro.token
                :<token>
                :<identifier>
                :token-text
                :token-line)
  (:import-from :cmacro.pattern
                :match-macro))
(in-package :cmacro.macroexpand)

(defparameter *found* nil
  "Was a macro found during the last macroexpansion?")

(defmethod macro-call-p ((token <token>) macros)
  (and (subtypep (type-of token) '<identifier>)
       (gethash (token-text token) macros) t))

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
                    (progn
                      (setf *found* t)
                      )
                    ;; The macro didn't match. Signal an error.
                    (error 'cmacro.error:bad-match
                           :name (token-text node)
                           :line (token-line node)))
               ;; Not a macro, let it go
               node)))))
