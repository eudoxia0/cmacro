(in-package :cl-user)
(defpackage :cmacro.parser
  (:use :cl :esrap))
(in-package :cmacro.parser)

(defrule octal-digits (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
(defrule dec-digit (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(defrule hex-digit (or dec-digit
                       #\a #\A #\b #\B #\c #\C #\d #\D #\e #\E #\f #\F))

(defrule whitespace (+ (or #\Space #\Tab #\Newline #\Linefeed))
  (:constant nil))

(defun not-doublequote (char)
  (not (eql #\" char)))

(defrule escape-string (and #\\ #\")
     (:constant nil))

(defrule string-char
    (or escape-string
        (not-doublequote character)))

(defrule string (and (? (or "u8" "u" "U" "L")) #\" (* string-char) #\")
  (:destructure (prefix q1 string q2)
    (declare (ignore prefix q1 q2))
    (text string)))
