(in-package :cl-user)
(defpackage cmacro.parse
  (:use :cl :anaphora))
(in-package :cmacro.parse)

(defstruct token
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
  '(("(" . :list-open)
    (")" . :list-close)
    ("[" . :array-open)
    ("]" . :array-close)
    ("{" . :block-open)
    ("}" . :block-close)))

(defun map-separator (sep)
  (cdr (assoc sep +separator-map+ :test #'equal)))

(defun process (data)
  (mapcar 
   #'(lambda (lexeme)
       (let ((tok-type (map-token-type (subseq lexeme 0 3)))
             (tok-text (subseq lexeme 4)))
         (aif (and (eql tok-type :op)
                   (map-separator tok-text))
              (make-token :type :op :text it)
              (make-token :type tok-type :text tok-text))))
   data))
