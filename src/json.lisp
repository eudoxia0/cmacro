(in-package :cmacro.parse)

(defmethod encode ((tok token) &optional (stream *standard-output*))
  (with-output (stream)
    (with-object ()
      (encode-object-element "type" (symbol-name (token-type tok)))
      (encode-object-element "text" (token-text tok)))))

(defun sexp-to-json (ast)
  "What it says on the tin."
  (let ((stream (make-string-output-stream)))
    (encode ast stream)
    (get-output-stream-string stream)))

(defun hash-to-token (hash-table)
  (make-token :type (intern (gethash "type" hash-table)
                            (find-package :keyword))
              :text (gethash "text" hash-table)))

(defun import-sexp (sexp)
  (loop for node in sexp collecting
        (if (listp node)
            (import-sexp node)
            (hash-to-token node))))

(defun json-to-ast (string)
  (import-sexp (yason:parse string)))
