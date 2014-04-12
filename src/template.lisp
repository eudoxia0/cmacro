;;;; cl-mustache customizations
(in-package :mustache)

;; We aren't producing HTML
(setf *char-to-escapes* "")

;;; Accessing the database

;; Storing
(defclass store-tag (non-standalone-tag)
  ((key :initarg :key :accessor key)
   (data :initarg :store :accessor store)))

(set-mustache-character
  #\<
  (lambda (raw-text arg-text escapep start end)
    (let* ((pos (position #\Space arg-text))
           (key (subseq arg-text 0 pos))
           (var-name (subseq arg-text (1+ pos))))
    (make-instance 'store-tag :key key :data var-name))))

(defmethod render-token ((token store-tag) context template)
  ;; Extract the named variable and store it
  (print-data "" t context))

(defclass retrieve-tag (non-standalone-tag)
  ((key :initarg :key :accessor key)
   (default :initarg :default :accessor default :initform nil)))

(set-mustache-character
  #\>
  (lambda (raw-text arg-text escapep start end)
    (let ((pos (position #\Space arg-text)))
      (if pos
          ;; We have a default value
          (let ((key (subseq arg-text 0 pos))
                (default (subseq arg-text (1+ pos))))
            (make-instance 'retrieve-tag :key key :default default))
          ;; Just retrieve the key
          (make-instance 'retrieve-tag :key key)))))

(defmethod render-token ((token retrieve-tag) context template)
  (print-data (cmacro.db:retrieve (key token) (default token)) t context))
