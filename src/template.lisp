;;;; cl-mustache customizations
(in-package :mustache)

;; We aren't producing HTML
(setf *char-to-escapes* "")

;; Generate symbols

(defclass gensym-tag (non-standalone-tag)
  ((label :initarg :label :accessor label)))

(set-mustache-character
  #\$
  (lambda (raw-text arg-text escapep start end)
    (make-instance 'gensym-tag :label arg-text)))

(defmethod render-token ((token gensym-tag) context template)
   (print-data (cmacro.db:gen-sym (label token)) t context))

;; Access generated symbols

(defclass getsym-tag (non-standalone-tag)
  ((label :initarg :label :accessor label)
   (num   :initarg :num   :accessor num)))

(set-mustache-character
  #\@
  (lambda (raw-text arg-text escapep start end)
    (let* ((pos (position #\/ arg-text))
           (label (subseq arg-text 0 pos))
           (num (parse-integer (subseq arg-text (1+ pos)))))
      (make-instance 'getsym-tag
                     :label label
                     :num num))))

(defmethod render-token ((token getsym-tag) context template)
  (print-data (cmacro.db:get-sym (label token) (num token)) t context))

;;; Accessing the database

;; Storing

#|
(defclass store-tag (non-standalone-tag)
  ((db-key :initarg :db-key :accessor db-key)
   (data :initarg :store :accessor store)))

(set-mustache-character
  #\<
  (lambda (raw-text arg-text escapep start end)
    (let* ((pos (position #\Space arg-text))
           (db-key (subseq arg-text 0 pos))
           (var-name (subseq arg-text (1+ pos))))
    (make-instance 'store-tag :db-key db-key :data var-name))))

(defmethod render-token ((token store-tag) context template)
  ;; Extract the named variable and store it
  (print-data "" t context))

(defclass retrieve-tag (non-standalone-tag)
  ((db-key :initarg :db-key :accessor db-key)
   (default :initarg :default :accessor default :initform nil)))

(set-mustache-character
  #\>
  (lambda (raw-text arg-text escapep start end)
    (let ((pos (position #\Space arg-text)))
      (if pos
          ;; We have a default value
          (let ((db-key (subseq arg-text 0 pos))
                (default (subseq arg-text (1+ pos))))
            (make-instance 'retrieve-tag :db-key db-key :default default))
          ;; Just retrieve the db-key
          (make-instance 'retrieve-tag :db-key db-key)))))

(defmethod render-token ((token retrieve-tag) context template)
  (print-data (cmacro.db:retrieve (db-key token) (default token)) t context))|#

(in-package :cl-user)
(defpackage cmacro.template
  (:use :cl)
  (:import-from :cmacro.var
                :var-name)
  (:export :render-template))
(in-package :cmacro.template)

(defun render-template (template variables)
  (mustache:mustache-render-to-string
   template
   (loop for pair in variables collecting
                               (cons (var-name (car pair))
                                     (cmacro.parse:print-ast (cdr pair))))))
