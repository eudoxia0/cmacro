(in-package :cl-user)
(defpackage cmacro.var
  (:use :cl)
  (:import-from :alexandria
                :flatten)
  (:import-from :cmacro.preprocess
                :+var-identifier+)
  (:import-from :cmacro.parse
                :token-type
                :token-text
                :token-equal)
  (:export :var
           :var-text
           :extract-vars))
(in-package :cmacro.var)

(defstruct var () text)

(defun parse-case (ast)
  "Extract variables from the AST of a case clause."
  (loop for sub-ast on (flatten ast) collecting
    (let ((node (first sub-ast)))
      (if (and (eq (token-type node) :ident)
               (equal (token-text node) +var-identifier+))
          ;; The next token is a variable
          (prog1
            (make-var :text (token-text (cadr sub-ast)))
            ;; Make sure we skip the actual var string
            (setf sub-ast (cdr sub-ast)))
          node))))

(defun extract-vars (ast)
  (remove-if #'null (parse-case ast)))

(defun match-token (case-tok input-tok)
  (if (var-p case-tok)
      (match-var case-tok input-tok)
      (token-equal case-tok input-tok)))
