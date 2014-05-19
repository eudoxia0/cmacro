(in-package :cl-user)
(defpackage cmacro.pattern
  (:use :cl :anaphora)
  (:import-from :cmacro.token
                :<token>
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
                :case-match))
(in-package :cmacro.pattern)

(defmethod var-p ((token <token>))
  "Is the token a variable?"
  (is (type-of token) '<variable>))

(defmethod match-group ((var <variable>) list)
  "Groups are lists, arrays and blocks. This checks whether var matches list."
  (let ((list-type (list-type list)))
    (when list-type
      (or (var-group-p var)
          (and (var-list-p var) (eq list-type :list))
          (and (var-array-p var) (eq list-type :array))
          (and (var-block-p var) (eq list-type :block))))))

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
