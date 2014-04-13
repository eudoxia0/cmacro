(in-package :cl-user)
(defpackage cmacro.var
  (:use :cl)
  (:import-from :alexandria
                :flatten)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :cmacro.preprocess
                :+var-identifier+)
  (:import-from :cmacro.parse
                :token-type
                :token-text
                :token-equal)
  (:export :make-var
           :var-name
           :var-qualifiers
           :extract-var
           :extract-vars
           :parse-template
           :match-var
           :match-token
           :match))
(in-package :cmacro.var)

(defstruct var name qualifiers)

(defparameter +var-type-map+
  '(("ident"  . :ident)
    ("const"  . :const)
    ("int"    . :integer)
    ("float"  . :float)
    ("num"    . :num)
    ("string" . :string)
    ("op"     . :op)))

(defun map-var-type (type)
  (cdr (assoc type +var-type-map+ :test #'equal)))

(defun extract-var (string)
  (let ((split (split-sequence #\: (remove #\Space (remove #\" string)))))
    (make-var :name (first split)
              :qualifiers (if (cdr split)
                            (append (list (map-var-type (cadr split)))
                                    (cddr split))))))

(defun restp (variable)
  (and (var-p variable)
       (eq (first (var-qualifiers variable)) :rest)))

(defun extract-vars (ast)
  "Extract variables from the AST of a case clause."
  (loop for sub-ast on ast collecting
    (let ((node (first sub-ast)))
      (if (listp node)
          (extract-vars node)
          (if (cmacro.parse:ident-eql node +var-identifier+)
              ;; The next token is a variable
              (prog1
                  (extract-var (token-text (cadr sub-ast)))
                ;; Make sure we skip the actual var string
                (setf sub-ast (cdr sub-ast)))
              node)))))
 
(defun parse-template% (ast)
  (loop for node in ast collecting
        (if (listp node)
            (parse-template% node)
            (if (var-p node)
                (cmacro.parse:make-token :type :ident
                                         :text (format nil
                                                       "{{~A}}"
                                                       (var-name node)))
                node))))

(defun parse-template (ast)
  "Template tags use the same syntax as variables, since raw cl-mustache tags
would be converted to blocks and that is such a hassle. So we extract variables
from a template clause, and turn them back to cl-mustache tags."
  (parse-template% (extract-vars ast)))

(defun eql-qualifier (qualifier token-type)
  (or (eq qualifier token-type)
      (and (eq qualifier :const)
           (or (eq token-type :integer)
               (eq token-type :float)
               (eq token-type :string)))
      (and (eq qualifier :num)
           (or (eq token-type :integer)
               (eq token-type :float)))))

(defun match-var (var token)
  (cond
    ((null (var-qualifiers var))
     ;; The variable accepts whatever
     t)
    ((eql-qualifier (first (var-qualifiers var))
                    (token-type token))
     ;; Qualifier match
     t)
    (t nil)))

(defun match-token (pattern input)
  (if (var-p pattern)
      (match-var pattern input)
      (token-equal pattern input)))

(defun append-bindings (pattern input bindings)
  (append bindings (list (cons pattern input))))

(defun append-rest-bindings (pattern input bindings)
  (append-bindings pattern (if (atom input) (list input) input) bindings))

(defun match% (pattern input &optional (bindings '(t)))
  (if bindings
      (cond
        ((and (atom pattern) (not (var-p pattern))
              (or (and (null pattern) (null input))
                  (match-token pattern input)))
         bindings)
        ((restp pattern)
         (append-rest-bindings pattern input bindings))
        ((var-p pattern)
         (append-bindings pattern input bindings))
        ((listp pattern)
         (if (restp (first pattern))
             (append-rest-bindings (first pattern) input bindings)
             (match% (rest pattern) (rest input)
                     (match% (first pattern) (first input) bindings)))))))

(defun match (pattern input)
  (if (and (listp pattern) (listp input))
      (if (> (length pattern) (length input))
          nil
          (match% pattern (subseq input 0 (length pattern))))
      (match% pattern input)))
