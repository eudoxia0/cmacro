(in-package :cl-user)
(defpackage cmacro.parse
  (:use :cl :anaphora)
  (:import-from :yason
                :encode
                :with-output
                :with-object
                :encode-object-element)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :alexandria
                :flatten)
  (:export :make-token
           :token-text
           :token-type
           :token-equal
           :ident-eql
           :parse-data
           :parse-pathname
           :print-ast
           :sexp-to-json
           :json-to-ast))
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
  (line 0   :type integer)
  (text ""  :type string))

(defun token-equal (a b)
  (and (eq (token-type a) (token-type b))
       (equal (token-text a) (token-text b))))

(defun ast-equal (ast-a ast-b)
  (let* ((ast-a (flatten ast-a))
         (ast-b (flatten ast-b))
         (len-a (length ast-a))
         (len-b (length ast-b)))
    (when (eql len-a len-b)
      ;; Compare individual items
      (loop for i from 0 to (1- len-a) do
        (if (not (token-equal (nth i ast-a)
                              (nth i ast-b)))
            (return nil)))
        t)))

(defun opening-token-p (tok)
  (member (token-text tok) +opening-separators+ :test #'equal))

(defun closing-token-p (tok)
  (member (token-text tok) +closing-separators+ :test #'equal))

(defun separator-token-p (tok)
  (member (token-text tok) +separators+ :test #'equal))

(defun parenp (tok)
  (or (equal tok "(") (equal tok ")")))

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
                 (or (null tok)
                     (null (token-type tok))))
             (mapcar 
              #'(lambda (lexeme)
                  (let* ((split (split-sequence #\: lexeme))
                         (tok-type (cdr (assoc (first split)
                                               +token-type-map+
                                               :test #'equal)))
                         (tok-line (aif (second split)
                                        (parse-integer it :junk-allowed t)))
                         (tok-text (third split)))
                    (if (and tok-type tok-text)
                        (make-token :type tok-type
                                    :line tok-line
                                    :text tok-text))))
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
