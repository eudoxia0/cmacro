(in-package :cmacro.parse)

(defun space-after-last-char-p (stream)
  "Take a stream, and determine whether we should print a space before the next
token."
  t)

(defun print-token (token stream)
  (if (and (not (separator-token-p token))
           (not (parenp token))
           (space-after-last-char-p stream))
    (write-char #\Space stream))
  ;; If it's a curly brace, also print a space before it
  (if (blockp token)
      (write-char #\Space stream))
  (write-string (token-text token)
                stream)
  (when (or (blockp token)
            (and (eq (token-type token) :op)
                 (equal (token-text token) ";")))
    (write-char #\Newline stream)))

(defun print-expression (expression stream)
  "Print an AST into a given stream."
  (if (listp expression)
      ;; Block
      (progn
        ;; Print the separator, then, if it's a curly brace, print
        ;; a newline
        (let ((sep (car expression)))
          (print-expression sep stream)
          (loop for item in (cdr expression) do
            (print-expression item stream))
          ;; Print the matching closing separator
          (aif (and (token-p sep)
                    (position (token-text sep)
                              +opening-separators+
                              :test #'equal))
               (print-expression
                (make-token :type :op :text (nth it +closing-separators+))
                stream))))
      ;; Regular token
      (print-token expression stream)))

(defun print-ast (ast)
  "Turn an AST into a list."
  (let ((stream (make-string-output-stream)))
    (print-expression ast stream)
    (get-output-stream-string stream)))
