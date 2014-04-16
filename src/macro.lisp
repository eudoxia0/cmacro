;;;; This file provides the core functionality of cmacro. It handles the
;;;; following:
;;;; * Extraction of `macro` forms from the AST
;;;; * Searching the AST for macro calls
;;;; * Macro expansion

(in-package :cl-user)
(defpackage cmacro.macro
  (:use :cl :anaphora)
  (:import-from :cmacro.preprocess
                :+var-identifier+)
  (:import-from :cmacro.parse
                :make-token
                :token-p
                :token-type
                :token-text
                :ident-eql
                :print-ast)
  (:import-from :cmacro.error
                :bad-macro-definition
                :bad-match)
  (:export :extract-macro-definitions
           :macroexpand-ast))
(in-package :cmacro.macro)

(defun block-text (block)
  (reduce #'(lambda (a b)
              (concatenate 'string a b))
          (loop for exp in (cdr block) collecting
            (cmacro.parse:print-ast exp))))

(defstruct macro-case match template toplevel external)

(defstruct macro cases)

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
           (push (cmacro.var:extract-vars (cdr code)) matching))
          ((ident-eql directive "template")
           (if template
               ;; Can't have two template directives in one case
               (error 'bad-macro-definition
                      :text "Repeated template directives.")
               (setf template (block-text (cmacro.var:parse-template code)))))
          ((ident-eql directive "toplevel")
           (if toplevel
               (error 'bad-macro-definition
                      :text "Repeated toplevel directives.")
               (setf toplevel (block-text (cmacro.var:parse-template code)))))
          ((ident-eql directive "external")
           (if external
               (error 'bad-macro-definition
                      :text "Repeated external directives.")
               (setf external (block-text code)))))))
    (when (and template external)
      (error 'bad-macro-definition
             :text "Can't have both template and external directives."))
    (make-macro-case :match matching
                     :template template
                     :toplevel toplevel
                     :external external)))

(defun parse-macro-definition (ast)
  (loop for sub-ast on ast by #'cddr collecting
    (let ((case-directive (first sub-ast))
          (case-code (cdr (second sub-ast))))
      (unless case-code
        (error 'bad-macro-definition
               :text "Uneven number of elements in macro definition."))
      (unless (ident-eql case-directive "case")
        (error 'bad-macro-definition
               :text "Unknown directive in macro definition."))
      (parse-case case-code))))

(defun void-token ()
  (cmacro.parse:make-token :type :ident :text "/**/"))

(defun extract-macro-definitions% (ast table)
  (loop for sub-ast on ast collecting
    (let ((node (first sub-ast)))
      (if (listp node)
          ;; Recur
          (extract-macro-definitions% node table)
          ;; Is it a macro definition?
          (if (ident-eql node "macro")
              ;; Parse the macro definition
              (progn
                (setf (gethash (token-text (cadr sub-ast)) table)
                      (make-macro :cases (parse-macro-definition (cdar (cddr sub-ast)))))
                ;; Remove the macro definition
                (setf sub-ast (cddr sub-ast))
                ;; Replace the macro with a comment
                (void-token))
              ;; Nope
              node)))))

(defun extract-macro-definitions (ast)
  (let ((table (make-hash-table :test #'equal)))
    (list (extract-macro-definitions% ast table)
          table)))

(defun macro-call-p (token macros)
  "Determine if an identifier is making a call to a macro."
  (and (token-p token)
       (gethash (token-text token) macros)))

(defun case-match (macro-case ast)
  (loop for pattern in (macro-case-match macro-case) do
    (let ((match (cmacro.var:match pattern ast)))
      (if (eq t (first match))
          ;; Successful match
          (return (list :bindings match
                        :length (length pattern)
                        :case macro-case))))))

(defun macro-match (macro ast)
  (loop for case in (macro-cases macro) do
    (let ((match (case-match case ast)))
      (if (listp match)
          (return match)))))

(defparameter *found* nil
  "t if a macro was expanded during the last macroexpansion.")
(defparameter *toplevel-expansions* (list))

(defun macroexpand-ast% (ast macros)
  (loop for sub-ast on ast collecting
    (let ((expression (first sub-ast)))
      (if (listp expression)
          ;; Recur
          (macroexpand-ast% expression macros)
          ;; An ordinary expression, possibly an identifier
          (aif (macro-call-p expression macros)
               ;; Expand the macro
               (aif (macro-match it (cdr sub-ast))
                    ;; The macro matches one of the clauses, so we replace the
                    ;; part of `sub-ast` that matched with the macro output
                    (progn
                      (setf *found* t)
                      (setf sub-ast (nthcdr (getf it :length) sub-ast))
                      (let* ((template (macro-case-template (getf it :case)))
                             (toplevel (macro-case-toplevel (getf it :case)))
                             (toplevel-render
                               (if toplevel
                                   (cmacro.parse:parse-data
                                    (cmacro.template:render-template
                                     toplevel
                                     (aif (getf it :bindings)
                                          (cdr it))))
                                   (void-token))))
                        (push toplevel-render *toplevel-expansions*)
                        (if template
                            (cmacro.parse:parse-data
                             (cmacro.template:render-template
                              template
                              (aif (getf it :bindings)
                                   (cdr it))))
                            (void-token))))                            
                    ;; The macro didn't match. Signal an error.
                    (error 'cmacro.error:bad-match :token expression))
               ;; Let it go
               expression)))))

(defun macroexpand-ast (ast macros)
  (let* ((ast (macroexpand-ast% ast macros)))
    (loop while *found* do
      (setf *found* nil)
      (setf ast (macroexpand-ast% ast macros))
      ;; Insert toplevel expansions
      (setf ast (append *toplevel-expansions* ast))
      (setf *toplevel-expansions* (list)))
    ast))
