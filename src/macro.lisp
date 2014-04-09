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
                :token-text
                :ident-eql
                :print-ast)
  (:import-from :cmacro.error
                :bad-macro-definition
                :bad-match))
(in-package :cmacro.macro)

(defun block-text (block)
  (print-ast (subseq block 1 (1- (length block)))))

(defun parse-case (ast)
  (let ((matching (list))
        template
        toplevel
        external)        
    (loop for sub-ast on ast by #'cddr do
      (let ((directive (first sub-ast))
            (code (second sub-ast)))
        (unless code
          (error 'bad-macro-definition
                 :text "Uneven number of elements in macro case."))
        (cond
          ((ident-eql directive "match")
           (push (block-text code) matching))
          ((ident-eql directive "template")
           (if template
               ;; Can't have two template directives in one case
               (error 'bad-macro-definition
                      :text "Repeated template directives.")
               (setf template (block-text code))))
          ((ident-eql directive "toplevel")
           (if toplevel
               (error 'bad-macro-definition
                      :text "Repeated toplevel directives.")
               (setf toplevel (block-text code))))
          ((ident-eql directive "external")
           (if external
               (error 'bad-macro-definition
                      :text "Repeated external directives.")
               (setf external (block-text code)))))))
    (when (and template external)
      (error 'bad-macro-definition
             :text "Can't have both template and external directives."))
    (list :matching matching
          :template template
          :toplevel toplevel
          :external external)))

(defun parse-macro-definition (ast)
  (loop for sub-ast on ast by #'cddr collecting
    (let ((case-directive (first sub-ast))
          (case-code (second sub-ast)))
      (unless case-code
        (error 'bad-macro-definition
               :text "Uneven number of elements in macro definition."))
      (unless (ident-eql case-directive "case")
        (error 'bad-macro-definition
               :text "Unknown directive in macro definition."))
      (parse-case case-code))))

(defun extract-macro-definitions% (ast)
  (loop for sub-ast on ast collecting
    (let ((node (first sub-ast)))
      (if (listp node)
          ;; Recur
          (extract-macro-definitions (cdr node))
          ;; Is it a macro definition?
          (if (ident-eql node "macro")
              ;; Parse the macro definition
              (parse-macro-definition (cadr sub-ast))
              ;; Nope
              nil)))))

(defun extract-macro-definitions (ast)
  (remove-if #'null (alexandria:flatten (extract-macro-definitions% ast))))

(defun macro-call-p (name macros)
  "Determine if an identifier is making a call to a macro."
  (gethash name macros))

#|
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
|#
