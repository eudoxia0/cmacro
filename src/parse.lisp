(in-package :cl-user)
(defpackage cmacro.parse
  (:use :cl :anaphora)
  (:export :parse))
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
               (format stream "~A" (token-text tok)))))
  type
  text)


(defun opening-token-p (tok)
  (member (token-text tok) +opening-separators+ :test #'equal))

(defun closing-token-p (tok)
  (member (token-text tok) +closing-separators+ :test #'equal))

(defun compound-token-p (tok)
  (member (token-text tok) +separators+ :test #'equal))


(defun process (lexemes)
  (mapcar 
   #'(lambda (lexeme)
       (let ((tok-type (assoc (subseq lexeme 0 3)
                              +token-type-map+
                              :test #'equal))
             (tok-text (subseq lexeme 4)))
         (make-token :type tok-type :text tok-text)))
   lexemes))

(defun parse (tokens)
  (let ((context (list nil)))
    (loop for tok in tokens do
      (if (compound-token-p tok)
          ;; Separator token
          (if (opening-token-p tok)
              ;; Opening token
              (push (list (token-text tok)) context)
              ;; Closing token
              (let ((cur-context (pop context)))
                (setf (first context)
                      (append (first context) (list cur-context)))))
          ;; Common token
          (setf (first context)
                (append (first context)
                        (list tok)))))
    (car context)))
