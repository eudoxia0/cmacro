(in-package :cl-user)
(defpackage :cmacro.parser
  (:use :cl :esrap)
  (:import-from :cmacro.token
                :<integer>
                :<identifier>
                :<string>
                :<operator>
                :<variable>)
  (:import-from :split-sequence
                :split-sequence)
  (:export :parse-string
           :parse-pathname))
(in-package :cmacro.parser)

(defparameter *text* nil
  "The current string being parsed. This is only used to find line numbers,
  which esrap doesn't keep track of (Just the absolute position).")

(defun line (position)
  "Line in *text* where `position` occurs."
  (if *text*
      (1+ (count #\Newline *text* :end position))
      -1))

(defun column (position)
  "Column in *text* where `position` occurs."
  (if *text*
      (- position (or (position #\Newline *text* :end position :from-end t)
                      0))
      -1))

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
  (:lambda (digits)
    (text digits)))

(defrule integer (and (or octal hex dec) (? integer-suffix))
  (:destructure (num suff &bounds start-pos)
    (declare (ignore suff))
    (make-instance '<integer> :text num :line (line start-pos))))

;;; Strings

(defun not-doublequote (char)
  (not (eql #\" char)))

(defrule escape-string (and #\\ #\")
  (:constant "\\\""))

(defrule string-char
    (or escape-string
        (not-doublequote character)))

(defrule string (and (? (or "u8" "u" "U" "L")) #\" (* string-char) #\")
  (:destructure (prefix q1 string q2 &bounds start-pos)
    (declare (ignore prefix q1 q2))
    (make-instance '<string>
                   :text (concatenate 'string q1 (text string) q2)
                   :line (line start-pos))))

;;; Identifiers

(defrule alphanumeric (alphanumericp character))

(defrule identifier (+ (or alphanumeric #\_))
  (:lambda (list &bounds start-pos)
    (make-instance '<identifier>
                   :text (text list)
                   :line (line start-pos))))

;;; Variables

(defrule var-char (not (or #\( #\))))

(defrule variable (and #\$ #\( (+ var-char) #\))
  (:destructure (dollar open text close &bounds start-pos)
    (let ((split (split-sequence #\Space (text text))))
      (make-instance '<variable>
                     :name (first split)
                     :qualifiers (rest split)
                     :line (line start-pos)))))

;;; Operators

(defun group-separatorp (char)
  (member char (list #\( #\) #\[ #\] #\{ #\}) :test #'char=))

(defrule group-separator (group-separatorp character))

(defrule op-char (not (or alphanumeric group-separator)))

(defrule operator (+ op-char)
  (:lambda (list &bounds start-pos)
    (make-instance '<operator>
                   :text (coerce list 'string)
                   :line (line start-pos))))

;;; Structure

(defrule atom (or  integer string identifier variable operator))

(defrule list (and #\( (* ast) #\))
  (:destructure (open items close)
    (cons :list (first items))))

(defrule array (and #\[ (* ast) #\])
  (:destructure (open items close)
    (cons :array (first items))))

(defrule block (and #\{ (* ast) #\})
  (:destructure (open items close)
    (cons :block (first items))))

(defrule ast (+ (and (? whitespace) (or macro atom list array block)))
  (:lambda (items)
    (mapcar #'(lambda (item) (second item)) items)))

;;; Macro definitions

(defmacro define-case-rule (rule-name rule-string)
  `(defrule ,rule-name (and (? whitespace) ,rule-string (? whitespace) #\{
                            ast (? whitespace) #\})
     (:destructure (ws1 label ws2 open ast ws3 close)
       (list ,(intern rule-string :keyword) ast))))

(define-case-rule macro-match "match")
(define-case-rule macro-template "template")
(define-case-rule macro-toplevel "toplevel")

(defrule macro-case (and (? whitespace) "case" (? whitespace) #\{
                         (* macro-match) macro-template (? macro-toplevel)
                         (? whitespace) #\})
  (:destructure (ws1 label ws2 open match template toplevel ws3 close)
    (list :case match template toplevel)))

(defrule macro (and "macro" (? whitespace) identifier (? whitespace) #\{
                    (+ macro-case) (? whitespace) #\})
  (:destructure (label ws1 name ws2 open cases ws3 close)
    (list :macro cases)))

;;; Functions

(defun parse-string% (string)
  (setf *text* string)
  (prog1 (parse 'ast string)
    (setf *text* nil)))

(defun parse-string (string)
  (handler-bind ((esrap-error
                   #'(lambda (condition)
                       (let ((text (esrap-error-text condition))
                             (position (esrap-error-position condition)))
                         (error 'cmacro.error:parser-error
                                :text text
                                :line (line position)
                                :column (column position))))))
    (parse-string% string)))

(defun slurp-file (path)
  ;; Credit: http://www.ymeme.com/slurping-a-file-common-lisp-83.html
  (with-open-file (stream path)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

(defun parse-pathname (pathname)
  (parse-string 'ast (slurp-file pathname)))
