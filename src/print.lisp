(in-package :cmacro.parse)

;; Utility functions

(defun make-string-buffer ()
  (make-array 0 :fill-pointer 0
                :adjustable t
                :element-type 'character))

(defun buffer-write-char (buf char)
  (vector-push-extend char buf))

(defun buffer-write-string (buf string)
  ;; Not the best solution, but the other one broke
  (loop for char across string do
    (vector-push-extend char buf)))

(defun print-token (token buffer)
  (if (or (and (not (separator-token-p token))
               (not (parenp token)))
          (identp token)
          ;; If it's a curly brace, also print a space before it
          (blockp token))
    (buffer-write-char buffer #\Space))
  (buffer-write-string buffer
                       (token-text token))
  (when (or (blockp token)
            (and (eq (token-type token) :op)
                 (equal (token-text token) ";"))
            (equal (token-text token) "/**/"))
    (buffer-write-char buffer #\Newline)))

(defun print-expression (expression buffer)
  "Print an AST into a given buffer."
  (if (listp expression)
      ;; Block
      (progn
        ;; Print the separator, then, if it's a curly brace, print
        ;; a newline
        (let ((sep (car expression)))
          (print-expression sep buffer)
          (loop for item in (cdr expression) do
            (print-expression item buffer))
          ;; Print the matching closing separator
          (aif (and (token-p sep)
                    (position (token-text sep)
                              +opening-separators+
                              :test #'equal))
               (print-expression
                (make-token :type :op :text (nth it +closing-separators+))
                buffer))))
      ;; Regular token
      (print-token expression buffer)))

(defun print-ast (ast)
  "Turn an AST into a list."
  (let ((buffer (make-string-buffer)))
    (print-expression ast buffer)
    buffer))
