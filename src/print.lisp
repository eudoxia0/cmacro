(in-package :cmacro.parse)

(defun print-expression (expression stream)
  "Print an AST into a given stream."
  (if (listp expression)
      ;; Block
      (progn
        ;; Print the separator, then, if it's a curly brace, print
        ;; a newline
        (print-expression (car expression) stream)
        (loop for item in (cdr expression) do
          (print-expression item stream))
        ;; Print the matching closing separator
        (aif (and (token-p (car expression))
                  (position (token-text (car expression))
                            +opening-separators+
                            :test #'equal))
             (print-expression
              (make-token :type :op :text (nth it +closing-separators+))
              stream)))
      ;; Regular token
      (progn
        (unless (separator-token-p expression)
          (write-char #\Space stream))
        (write-string (token-text expression)
                      stream)
        (when (or (blockp expression)
                  (and (eq (token-type expression) :op)
                       (equal (token-text expression) ";")))
          (write-char #\Newline stream)))))

(defun print-ast (ast)
  "Turn an AST into a list."
  (let ((stream (make-string-output-stream)))
    (print-expression ast stream)
    (get-output-stream-string stream)))
