(in-package :cl-user)
(defpackage cmacro.parse
  (:use :cl :anaphora)
  (:export :make-token
           :token-text
           :token-type
           :ident-eql
           :parse-data
           :parse-pathname
           :print-ast))
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
  type
  text)


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
  (mapcar 
   #'(lambda (lexeme)
       (let ((tok-type (cdr (assoc (subseq lexeme 0 3)
                                   +token-type-map+
                                   :test #'equal)))
             (tok-text (subseq lexeme 4)))
         (make-token :type tok-type :text tok-text)))
   lexemes))

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
        (print (car expression))
        (print-expression (nth (position (car expression)
                                         +opening-separators+
                                         :test #'equal)
                               +closing-separators+)
                          stream))
      ;; Regular token
      (progn
        (write-string (token-text expression)
                      stream)
        (unless (separator-token-p expression)
          (write-char #\Space stream))
        (when (blockp expression)
          (write-char #\Newline stream)))))

(defun print-ast (ast)
  "Turn an AST into a list."
  (let ((stream (make-string-output-stream)))
    (print-expression ast stream)
    (get-output-stream-string stream)))
