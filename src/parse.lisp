(in-package :cl-user)
(defpackage cmacro.parse
  (:use :cl :anaphora)
  (:import-from :yason
                :encode
                :with-output
                :with-object
                :encode-object-element)
  (:export :make-token
           :token-text
           :token-type
           :token-equal
           :ident-eql
           :parse-data
           :parse-pathname
           :print-ast
           :ast-to-json))
(in-package :cmacro.parse)

(defparameter +token-type-map+
  '(("idn" . :ident)
    ("int" . :integer)
    ("flt" . :float)
    ("str" . :string)
    ("opr" . :op)))

(defparameter +opening-separators+ (list "(" "[" "{"))
(defparameter +closing-separators+ (list ")" "]" "}"))
(defparameter +separators+ (union +opening-separators+
                                  +closing-separators+))

(defstruct (token
            (:print-function
             (lambda (tok stream d)
               (declare (ignore d))
               (write-string (token-text tok) stream))))
  (type nil :type symbol)
  (text ""  :type string))

(defun token-equal (a b)
  (and (eq (token-type a) (token-type b))
       (equal (token-text a) (token-text b))))

(defun opening-token-p (tok)
  (member (token-text tok) +opening-separators+ :test #'equal))

(defun closing-token-p (tok)
  (member (token-text tok) +closing-separators+ :test #'equal))

(defun separator-token-p (tok)
  (member (token-text tok) +separators+ :test #'equal))

(defun blockp (tok)
  (or (equal (token-text tok) "{")
      (equal (token-text tok) "}")))

(defun ident-eql (tok text)
  (and (eq (token-type tok) :ident)
       (equal text (token-text tok))))

(defun process (lexemes)
  "Turn a list of lexemes into a list of tokens. Each lexeme is of the form:
    '[three letter type identifier]:[text]'"
  (declare (type list lexemes))
  (remove-if #'(lambda (tok)  ;; I am not entirely sure why null tokens happen
                 (null (token-type tok)))
             (mapcar 
              #'(lambda (lexeme)
                  (let ((tok-type (cdr (assoc (subseq lexeme 0 3)
                                              +token-type-map+
                                              :test #'equal)))
                        (tok-text (subseq lexeme 4)))
                    (make-token :type tok-type :text tok-text)))
              lexemes)))

(defun parse (tokens)
  "Parse a flat list of tokens into a nested data structure."
  (let ((context (list nil)))
    (loop for tok in tokens do
      (if (separator-token-p tok)
          ;; Separator token
          (if (opening-token-p tok)
              ;; Opening token
              (push (list tok) context)
              ;; Closing token
              (let ((cur-context (pop context)))
                (setf (first context)
                      (append (first context)
                              (list cur-context)))))
          ;; Common token
          (setf (first context)
                (append (first context)
                        (list tok)))))
    (car context)))


(defun parse-data (data)
  (parse (process (cmacro.preprocess:process-data data))))

(defun parse-pathname (pathname)
  (parse (process (cmacro.preprocess:process-pathname pathname))))


(defun print-expression (expression stream)
  "Print an AST into a given stream."
  (if (listp expression)
      ;; Block
      (progn
        ;; Print the separator, then, if it's a curly brace, print
        ;; a newline
        (print-expression (car expression) stream)
        (loop for item in (cdr expression) do
          (print-expression item stream))
        ;; Print the matching closing separator
        (aif (and (token-p (car expression))
                  (position (token-text (car expression))
                            +opening-separators+
                            :test #'equal))
             (print-expression
              (make-token :type :op :text (nth it +closing-separators+))
              stream)))
      ;; Regular token
      (progn
        (unless (separator-token-p expression)
          (write-char #\Space stream))
        (write-string (token-text expression)
                      stream)
        (when (or (blockp expression)
                  (and (eq (token-type expression) :op)
                       (equal (token-text expression) ";")))
          (write-char #\Newline stream)))))

(defun print-ast (ast)
  "Turn an AST into a list."
  (let ((stream (make-string-output-stream)))
    (print-expression ast stream)
    (get-output-stream-string stream)))

(defmethod encode ((tok token) &optional (stream *standard-output*))
  (with-output (stream)
    (with-object ()
      (encode-object-element "type" (symbol-name (token-type tok)))
      (encode-object-element "text" (token-text tok)))))

(defun ast-to-json (ast)
  "What it says on the tin."
  (encode ast))
