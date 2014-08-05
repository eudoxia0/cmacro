(in-package :cl-user)
(defpackage cmacro.template
  (:use :cl :anaphora)
  (:import-from :cmacro.token
                :token-text
                :<identifier>
                :<string>
                :<variable>
                :var-name
                :var-p
                :var-qualifiers
                :var-command-p
                :make-variable)
  (:import-from :cmacro.printer
                :print-ast)
  (:import-from :cmacro.db
                :gen-sym
                :get-sym)
  (:export :render-template))
(in-package :cmacro.template)

(defmethod get-var ((var <variable>) bindings)
  (gethash (var-name var) bindings))

(defmethod render-var ((var <variable>) bindings)
  (aif (get-var var bindings)
       it
       (error 'cmacro.error:unknown-var :var-name (var-name var))))

(defun render-command (command args bindings)
  (cond
    ((equal command "@gensym")
     (make-instance '<identifier>
                    :text (gen-sym (first args))))
    ((equal command "@getsym")
     ;; Get a symbol from a label, an optional `n` places back
     (make-instance '<identifier>
                    :text
                    (aif (second args)
                         (get-sym (first args)
                                  (parse-integer (second args)))
                         (get-sym (first args)))))
    ((equal command "@to-string")
     (make-instance '<string>
                    :text (concatenate 'string
                                       "\""
                                       (print-ast
                                        (get-var (make-variable (first args))
                                                 bindings))
                                       "\"")))
    ((equal command "@embed")
     (make-instance '<identifier>
                    :text (format nil "$(~{~A~#[~:; ~]~})" args)))
    ((equal command "@splice")
     (list :splice (rest (get-var (make-variable (first args))
                                  bindings))))
    ((equal command "@conc")
     (let* ((components (loop for arg in args collecting
                          (aif (gethash arg bindings)
                               (token-text it)
                               arg)))
            (text (reduce #'(lambda (str-a str-b)
                              (concatenate 'string str-a str-b))
                          components)))
            (make-instance '<identifier>
                           :text text)))
    (t
     (error 'cmacro.error:unknown-template-command :command command))))

(defmethod render-template-expression ((var <variable>) bindings)
  (if (var-command-p var)
      (render-command (var-name var) (var-qualifiers var) bindings)
      (render-var var bindings)))

(defun render-template (ast bindings)
  (loop for sub-ast on ast collecting
    (let ((node (first sub-ast)))
      (if (listp node)
          (render-template node bindings)
          (if (var-p node)
              (let ((out (render-template-expression node bindings)))
                (if (and (listp out)
                         (keywordp (first out))
                         (eql (first out) :splice))
                    ;; Splice out into sub-ast
                    (progn
                      (setf sub-ast (append (first (rest out))
                                            (rest sub-ast)))
                      (first sub-ast))
                    out))
              node)))))
