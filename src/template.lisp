(in-package :cl-user)
(defpackage cmacro.template
  (:use :cl :anaphora)
  (:import-from :cmacro.token
                :<variable>
                :var-name
                :var-p)
  (:export :render-template))
(in-package :cmacro.template)

#|
(defun run-template-command (command args)
  (cond
    ((equal command "gensym")
     ;; Generate a symbol, and associate it with a label
     (cmacro.db:gen-sym (first args)))
    ((equal command "getsym")
     ;; Get a symbol from a label, an optional `n` places back
     (aif (second args)
          (cmacro.db:get-sym (first args) (parse-integer (second args)))
          (cmacro.db:get-sym (first args))))
    ((equal command "to-string")
     (format nil "~S" (get-var-value (first args))))
    ((equal command "splice")
     (let ((block (first (cmacro.parse:parse-data (get-var-value (first args))))))
       (print block)
       (cmacro.parse:print-ast
        (if (listp block)
            (append (list (cmacro.parse:make-token :type :op :text ""))
                    (rest block))
            block))))
    ((equal command "embed")
     (format nil "$(~{~A ~})" args))
    (t
     (error 'cmacro.error:unknown-template-command :command command))))|#

(defmethod get-var ((var <variable>) bindings)
  (cadr (assoc var bindings :test #'(lambda (v1 v2) (equal (var-name v1)
                                                           (var-name v2))))))

(defmethod render-var ((var <variable>) bindings)
  (aif (get-var var bindings)
       it
       (error 'cmacro.error:unknown-var :var-name (var-name var))))

(defmethod render-template-expression ((var <variable>) bindings)
  (if (var-command-p var)
      (render-command (var-name var) (var-qualifiers var) bindings)
      (render-var var bindings)))

(defun render-template (ast bindings)
  (loop for node in ast collecting
        (if (listp node)
            (render-template node bindings)
            (if (var-p node)
                (render-template-expression node bindings)
                node))))
