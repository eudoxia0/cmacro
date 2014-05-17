(in-package :cl-user)
(defpackage :cmacro.parser
  (:use :cl :esrap))
(in-package :cmacro.parser)

;;; Whitespace

(defrule whitespace (+ (or #\Space #\Tab #\Newline #\Linefeed))
  (:constant nil))

;;; Numbers

;; Digits
(defrule octal-digit (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))

(defrule dec-digit (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defrule hex-digit (or dec-digit
                       #\a #\A #\b #\B #\c #\C #\d #\D #\e #\E #\f #\F))

;; Suffixes

(defrule integer-suffix (+ (or #\l #\L #\u #\U))
  (:constant nil))

(defrule float-suffix (or #\f #\F #\l #\L)
  (:constant nil))

;; Kinds of numbers

(defrule octal (and #\0 (+ octal-digit))
  (:destructure (o digits)
    (text o digits)))

(defrule hex (and #\0 (or #\x #\X) (+ hex-digit))
  (:destructure (o x digits)
    (text o x digits)))

(defrule dec (and (+ dec-digit))
  (:destructure (digits)
    (text digits)))

(defrule integer (and (or octal hex dec) (? integer-suffix))
  (:destructure (num suff)
    (declare (ignore suff))
    num))

;;; Strings

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
