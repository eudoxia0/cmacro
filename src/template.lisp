(in-package :cl-user)
(defpackage cmacro.template
  (:use :cl :anaphora)
  (:import-from :cmacro.token
                :<identifier>
                :<variable>
                :var-name
                :var-p
                :var-qualifiers
                :var-command-p)
  (:import-from :cmacro.db
                :gen-sym
                :get-sym)
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
                         (get-sym (first args) (parse-integer (second args)))
                         (get-sym (first args)))))
    (t
     (error 'cmacro.error:unknown-template-command :command command))))

(defmethod get-var ((var <variable>) bindings)
  (gethash (var-name var) bindings))

(defmethod render-var ((var <variable>) bindings)
  (aif (get-var var bindings)
       it
       (error 'cmacro.error:unknown-var :var-name (var-name var))))

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
