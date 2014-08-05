(in-package :cl-user)
(defpackage cmacro.pattern
  (:use :cl :anaphora)
  (:import-from :cmacro.token
                :<token>
                :token-equal
                :<number>
                :<integer>
                :<real>
                :<identifier>
                :<character>
                :<string>
                :<operator>
                :<variable>
                :var-name
                :var-rest-p
                :var-qualifiers
                :list-type
                :var-p
                :rest-p
                :atomic-var-p
                :var-list-p
                :var-array-p
                :var-block-p
                :var-group-p
                :var-has-qualifier)
  (:import-from :cmacro.macro
                :<macro-case>
                :case-match
                :<macro>
                :macro-cases)
  (:export :equal-bindings
           :match
           :match-macro
           :<match>
           :match-bindings
           :match-length
           :match-macro-case))
(in-package :cmacro.pattern)

;;; Bindings

(defun append-bindings (pattern input bindings)
  (append bindings (list (list pattern input))))

(defun append-rest-bindings (pattern input bindings)
  (append-bindings pattern (list :splice
                                 (if (atom input) (list input) input)) bindings))

;;; Pattern matching

(defmethod match-group ((var <variable>) list)
  "Groups are lists, arrays and blocks. This checks whether var matches list."
  (let ((list-type (list-type list)))
    (when list-type
      (if (or (var-group-p var)
              (and (var-list-p var) (eq list-type :list))
              (and (var-array-p var) (eq list-type :array))
              (and (var-block-p var) (eq list-type :block)))
          list))))

(defmacro qualifier-match (var input types qualifier)
  `(and (var-has-qualifier ,var ,qualifier)
        (or ,@(mapcar #'(lambda (type)
                          `(typep ,input ',type))
                      types))))

(defmethod match-var ((var <variable>) input bindings)
  (cond
    ((null (var-qualifiers var))
     ;; The variable accepts whatever
     (append-bindings var input bindings))
    ((and (listp input) (first input))
     ;; Match the variable to the list
     (if (match-group var input)
         (append-bindings var input bindings)
         nil))
    ;; Atomic qualifier matches
    ((or (qualifier-match var input (<identifier>) "ident")
         (qualifier-match var input (<integer>) "int")
         (qualifier-match var input (<real>) "float")
         (qualifier-match var input (<integer> <real>) "num")
         (qualifier-match var input (<string>) "string")
         (qualifier-match var input (<integer> <real> <string>) "const"))
     (append-bindings var input bindings))
    (t
     ;; Didn't match anything
     nil)))

(defun match% (pattern input &optional (bindings '(t)))
  (if bindings
      (if (atom pattern)
          (if (var-p pattern)
              ;; Variable: Is it atomic or rest?
              (if (rest-p pattern)
                  ;; Match the rest of the input
                  (append-rest-bindings pattern input bindings)
                  ;; Look at the variable's qualifiers and match it
                  (match-var pattern input bindings))
              ;; Just a token. Does it equal the input?
              (if (and (null pattern) (null input))
                  bindings
                  (if (atom input)
                       (if (and (subtypep (type-of pattern) '<token>)
                                (subtypep (type-of input) '<token>))
                           (if (token-equal pattern input)
                               bindings)
                           (if (and (keywordp pattern) (keywordp input)
                                    (eql pattern input))
                               bindings)))))
          (if (rest-p (first pattern))
              (append-rest-bindings (first pattern) input bindings)
              (match% (rest pattern) (rest input)
                      (match% (first pattern) (first input) bindings))))))

(defun bindings->hash-table (bindings)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (variable value) in bindings do
      (setf (gethash (var-name variable) table)
            value))
    table))

(defun equal-bindings (table-a table-b)
  (if (eql (hash-table-size table-a) (hash-table-size table-b))
      (progn
        (loop for key being the hash-keys of table-a do
          (unless (equal (gethash key table-a) (gethash key table-b))
            (return nil)))
        t)))

(defun match (pattern input)
  (let ((bindings
          (if (and (listp pattern) (listp input))
              (if (> (length pattern) (length input))
                  nil
                  (match% pattern (subseq input 0 (length pattern))))
              (match% pattern input))))
    (when bindings
      (list :bindings (bindings->hash-table (rest bindings))
            :length (if (listp pattern) (length pattern) 1)))))

;;; Match against a macro

(defclass <match> ()
  ((bindings :initarg :bindings
             :reader match-bindings)
   (length :initarg :length
           :reader match-length
           :type integer)
   (case :initarg :case
         :reader match-macro-case
         :type <macro-case>)))

(defmethod match-case ((case <macro-case>) input)
  (loop for case-match in (case-match case) do
    (aif (match case-match input)
         (return (make-instance '<match>
                                :bindings (getf it :bindings)
                                :length (getf it :length)
                                :case case)))))

(defmethod match-macro ((macro <macro>) input)
  (loop for case in (macro-cases macro) do
    (aif (match-case case input)
         (return it))))
