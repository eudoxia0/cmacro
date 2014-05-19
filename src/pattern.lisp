(in-package :cl-user)
(defpackage cmacro.pattern
  (:use :cl :anaphora)
  (:import-from :cmacro.token
                :<token>
                :token-equal
                :<variable>
                :var-rest-p
                :var-qualifiers
                :list-type
                :var-list-p
                :var-array-p
                :var-block-p
                :var-group-p)
  (:import-from :cmacro.macro
                :<macro-case>
                :case-match)
  (:export :match-bindings
           :match-length))
(in-package :cmacro.pattern)

(defun var-p (pattern)
  (eq (type-of pattern) '<variable>))

(defun rest-p (pattern)
  (and (var-p pattern) (var-rest-p pattern)))

(defmethod match-group ((var <variable>) list)
  "Groups are lists, arrays and blocks. This checks whether var matches list."
  (let ((list-type (list-type list)))
    (when list-type
      (if (or (var-group-p var)
              (and (var-list-p var) (eq list-type :list))
              (and (var-array-p var) (eq list-type :array))
              (and (var-block-p var) (eq list-type :block)))
          t))))

(defmethod match-var ((var <variable>) input)
  (cond
    ((null (var-qualifiers var))
     ;; The variable accepts whatever
     t)
    ((and (listp input) (first input))
     ;; Match the variable to the list
     (match-group var input))
    ;((eql-qualifier (first (var-qualifiers var))
    ;                (token-type input))
    ; ;; Qualifier match
    ; t)
    (t
     ;; Didn't match anything
     nil)))

(defmethod match-token ((pattern <token>) input)
  (if (var-p pattern)
      (match-var pattern input)
      (token-equal pattern input)))

(defun append-bindings (pattern input bindings)
  (append bindings (list (list pattern input))))

(defun append-rest-bindings (pattern input bindings)
  (append-bindings pattern (if (atom input) (list input) input) bindings))

(defun match% (pattern input &optional (bindings '(t)))
  (if bindings
      (cond
        ((and (atom pattern) (atom input) (not (var-p pattern))
              (match-token pattern input))
         bindings)
        ((rest-p pattern)
         (append-rest-bindings pattern input bindings))
        ((var-p pattern)
         (append-bindings pattern input bindings))
        ((listp pattern)
         (if (rest-p (first pattern))
             (append-rest-bindings (first pattern) input bindings)
             (match% (rest pattern) (rest input)
                     (match% (first pattern) (first input) bindings)))))))

(defclass <match> ()
  ((bindings :initarg :bindings
             :reader match-bindings)
   (length :initarg :length
           :reader match-length
           :type integer)))

(defun match (pattern input)
  (let ((bindings
          (if (and (listp pattern) (listp input))
              (if (> (length pattern) (length input))
                  nil
                  (match% pattern (subseq input 0 (length pattern))))
              (match% pattern input))))
    (when bindings
      (make-instance '<match>
                     :bindings (rest bindings)
                     :length (length pattern)))))
