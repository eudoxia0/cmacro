(in-package :cl-user)
(defpackage cmacro.parse
  (:use :cl :anaphora)
  (:export :parse))
(in-package :cmacro.parse)

(defstruct (token
            (:print-function
             (lambda (tok stream d)
               (declare (ignore d))
               (format stream "~A" (token-text tok)))))
  type
  text)

(defparameter +token-type-map+
  '(("idn" . :ident)
    ("int" . :integer)
    ("flt" . :float)
    ("str" . :string)
    ("opr" . :op)))

(defun map-token-type (tok-type)
  (cdr (assoc tok-type +token-type-map+ :test #'equal)))

(defparameter +separator-map+
  '(("(" . (:list . :open))
    (")" . (:list . :close))
    ("[" . (:array . :open))
    ("]" . (:array . :close))
    ("{" . (:block . :open))
    ("}" . (:block . :close))))

(defun map-separator (sep)
  (cdr (assoc sep +separator-map+ :test #'equal)))

(defun process (lexemes)
  (mapcar 
   #'(lambda (lexeme)
       (let ((tok-type (map-token-type (subseq lexeme 0 3)))
             (tok-text (subseq lexeme 4)))
         (aif (and (eq tok-type :op)
                   (map-separator tok-text))
              (make-token :type :op :text it)
              (make-token :type tok-type :text tok-text))))
   lexemes))

(defun parse (tokens)
  (let ((context (list nil)))
    (loop for tok in tokens do
      (if (listp (token-text tok))
          ;; Separator token
          (if (eq :open (cdr (token-text tok)))
              ;; Opening token
              (push (list (car (token-text tok))) context)
              ;; Closing token
              (let ((cur-context (pop context)))
                (setf (first context)
                      (append (first context) (list cur-context)))))
          ;; Common token
          (setf (first context)
                (append (first context)
                        (list tok)))))
    (car context)))
