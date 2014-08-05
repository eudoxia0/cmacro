(in-package :cl-user)
(defpackage :cmacro.parser
  (:use :cl :esrap)
  (:import-from :cmacro.token
                :<text-token>
                :token-text
                :token-line
                :token-equal
                :<void-token>
                :<integer>
                :<real>
                :<identifier>
                :<character>
                :<string>
                :<operator>
                :<preproc>
                :<variable>
                :make-variable)
  (:import-from :cmacro.macro
                :<macro-case>
                :<macro>
                :macro-name)
  (:export :<result>
           :result-ast
           :result-macros
           :extract-macros
           :parse-string
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

(defun slurp-file (path)
  ;; Credit: http://www.ymeme.com/slurping-a-file-common-lisp-83.html
  (with-open-file (stream path)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

;;; Whitespace

(defparameter +whitespace-chars+
  (list #\Space #\Tab #\Newline #\Linefeed #\Backspace
        #\Page #\Return #\Rubout))

(defrule whitespace (+ (or #\Space #\Tab #\Newline #\Linefeed #\Backspace
                           #\Page #\Return #\Rubout))
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

(defrule sign (or #\+ #\-))

(defrule real (and (? sign)                 ;; Sign
                   (* dec-digit)            ;; Integer part
                   (and #\.                 ;; Fractional part
                        (? (or #\e #\E)) ;; Exponent
                        (? sign)
                        dec))
  (:lambda (items &bounds start-pos)
    (make-instance '<real>
                   :text (text items)
                   :line (line start-pos))))

(defrule number (or real integer))

;;; Strings and Characters

(defun not-doublequote (char)
  (not (eql #\" char)))

(defrule escape-string (and #\\ #\")
  (:constant "\\\""))

(defrule string-char
    (or escape-string
        (not-doublequote character)))

(defrule string (and (? (or "u8" "u" "U" "L")) #\" (* string-char) #\")
  (:destructure (prefix q1 string q2 &bounds start-pos)
    (declare (ignore prefix))
    (make-instance '<string>
                   :text (concatenate 'string q1 (text string) q2)
                   :line (line start-pos))))

;; TODO: This is stupid
(defrule char (and (? (or "u8" "u" "U" "L")) #\' (* string-char) #\')
  (:destructure (prefix q1 string q2 &bounds start-pos)
    (declare (ignore prefix))
    (make-instance '<character>
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
    (declare (ignore dollar open close))
    (make-variable (text text) (line start-pos))))

;;; Operators

(defun group-separatorp (char)
  (member char (list #\( #\) #\[ #\] #\{ #\}) :test #'char=))

(defrule group-separator (group-separatorp character))

(defrule op-char (not (or alphanumeric group-separator whitespace)))

(defrule operator (+ op-char)
  (:lambda (list &bounds start-pos)
    (make-instance '<operator>
                   :text (coerce list 'string)
                   :line (line start-pos))))

;;; Preprocessor directives

(defrule preproc-char (not #\Newline))

(defrule preproc (and #\# (* preproc-char) #\Newline)
  (:destructure (hash chars newline &bounds start-pos)
    (declare (ignore hash newline))
    (make-instance '<preproc>
                   :text (concatenate 'string "#" (text chars))
                   :line (line start-pos))))

;;; Structure

(defrule atom (or number char string preproc identifier variable operator))

(defrule list (and #\( (* ast) (? whitespace) #\))
  (:destructure (open items ws close)
    (declare (ignore open ws close))
    (cons :list (first items))))

(defrule array (and #\[ (* ast) (? whitespace) #\])
  (:destructure (open items ws close)
    (declare (ignore open ws close))
    (cons :array (first items))))

(defrule block (and #\{ (* ast) (? whitespace) #\})
  (:destructure (open items ws close)
    (declare (ignore open ws close))
    (cons :block (first items))))

(defrule ast (+ (and (? whitespace) (or macro atom list array block)))
  (:lambda (items)
    (mapcar #'(lambda (item) (second item)) items)))

;;; Macro definitions

(defmacro define-case-rule (rule-name rule-string)
  `(defrule ,rule-name (and (? whitespace) ,rule-string (? whitespace) #\{
                            ast (? whitespace) #\})
     (:destructure (ws1 label ws2 open ast ws3 close)
       (declare (ignore ws1 label ws2 open ws3 close))
       ast)))

(define-case-rule macro-match "match")
(define-case-rule macro-template "template")
(define-case-rule macro-toplevel "toplevel")

(defrule macro-case (and (? whitespace) "case" (? whitespace) #\{
                         (* macro-match) macro-template (? macro-toplevel)
                         (? whitespace) #\})
  (:destructure (ws1 label ws2 open match template toplevel ws3 close)
    (declare (ignore ws1 label ws2 open ws3 close))
    (make-instance '<macro-case>
                   :match match
                   :template template
                   :toplevel-template toplevel)))

(defrule macro (and "macro" (? whitespace) identifier (? whitespace) #\{
                    (+ macro-case) (? whitespace) #\})
  (:destructure (label ws1 name ws2 open cases ws3 close)
    (declare (ignore label ws1 ws2 open ws3 close))
    (make-instance '<macro> :name (token-text name)
                            :cases cases)))

;;; Functions

(defun parse-string% (string)
  (setf *text* string)
  (prog1 (parse 'ast string)
    (setf *text* nil)))

(defun remove-backslash-escapes (string)
  (let* ((target (concatenate 'string (string #\\) (string #\Newline)))
         (str string)
         (pos (search target string)))
    (loop while pos do
      (setf str (concatenate 'string
                             (subseq str 0 pos)
                             (subseq str (+ 2 pos))))
      (setf pos (search target str)))
    str))

(defun preprocess-string (string)
  (remove-backslash-escapes
   (string-trim +whitespace-chars+ string)))

(defun string-text (str)
  "Remove the quotes around a string."
  (subseq str 1 (1- (length str))))

(defun include-files (ast)
  "Go through the ast, looking for calls to the 'cmacro_import' macro."
  (loop for sub-ast on ast collecting
    (let ((node (first sub-ast)))
      (if (listp node)
          (include-files node)
          (if (and (typep node '<text-token>)
                   (token-equal node (make-instance '<identifier>
                                                    :text "cmacro_import")))
              (progn
                (let ((path (second sub-ast)))
                  ;; Remove the macro
                  (setf sub-ast (cddr sub-ast))
                  (if (typep path '<string>)
                      (parse-string
                       (slurp-file (parse-namestring (string-text
                                                      (token-text path)))))
                      (error 'cmacro.error:bad-import
                             :line (token-line node)))))
              node)))))

(defun parse-string (string)
  (handler-bind ((esrap-error
                   #'(lambda (condition)
                       (let ((text (esrap-error-text condition))
                             (position (esrap-error-position condition)))
                         (error 'cmacro.error:parser-error
                                :text text
                                :line (line position)
                                :column (column position))))))
    (include-files (parse-string% (preprocess-string string)))))

;;; Extract macros

(defclass <result> ()
  ((ast :reader result-ast
        :initarg :ast)
   (macros :reader result-macros
           :initarg :macros)))

(defun extract-macros (ast)
  (let ((table (make-hash-table :test #'equal)))
    ;; Go through the AST, looking for instances of macros, removing them from
    ;; the tree and and adding them to a hash table. Then return the table and
    ;; AST.
    (labels ((extract-macros% (ast)
               (loop for sub-ast on ast collecting
                 (let ((node (first sub-ast)))
                   (if (listp node)
                       (extract-macros% node)
                       (if (eq (type-of node) '<macro>)
                           (progn
                             (setf (gethash (macro-name node) table)
                                   node)
                             (make-instance '<void-token>))
                           node))))))
      (make-instance '<result>
                     :ast (extract-macros% ast)
                     :macros table))))

(defun parse-pathname (pathname)
  (parse-string (slurp-file pathname)))
