;;;; cl-mustache customizations
(in-package :cl-user)
(defpackage cmacro.template
  (:use :cl :anaphora)
  (:import-from :cmacro.var
                :var-name)
  (:import-from :mustache
                :*char-to-escapes*
                :non-standalone-tag
                :set-mustache-character
                :render-token
                :print-data)
  (:export :render-template))
(in-package :cmacro.template)
;; We aren't producing HTML
(setf *char-to-escapes* "")

;; General command tag

(defclass command-tag (non-standalone-tag)
  ((command :initarg :command :accessor command)
   (args    :initarg :args    :accessor args)))

(set-mustache-character
  #\@
  (lambda (raw-text arg-text escapep start end)
    (let ((split (split-sequence:split-sequence #\Space arg-text)))
      (make-instance 'command-tag :command (first split) :args (rest split)))))

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
    (t
     (error 'cmacro.error:unknown-template-command :command command))))

(defmethod render-token ((token command-tag) context template)
   (print-data (run-template-command (command token)
                                     (args token))
               t context))

(defun render-template (template variables)
  (mustache:mustache-render-to-string
   template
   (loop for pair in variables collecting
                               (cons (var-name (car pair))
                                     (cmacro.parse:print-ast (cdr pair))))))
