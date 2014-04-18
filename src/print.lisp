(in-package :cmacro.parse)

;; Utility functions

(defun make-string-buffer ()
  (make-array 0 :fill-pointer 0
                :adjustable t
                :element-type 'character))

(defun buffer-write-char (buf char)
  (vector-push-extend char buf))

(defun buffer-write-string (buf string)
  (let ((rest-string (subseq string 1)))
    (vector-push-extend (elt string 0) buf (1+ (length string)))
    (loop for char across rest-string do
      (vector-push char buf))))

(defun space-after-last-char-p (buffer)
  "Take a buffer, and determine whether we should print a space before the next
token."
  (if (> (length buffer) 0)
      (let ((last-char (elt buffer (1- (length buffer)))))
        t)))

(defun print-token (token buffer)
  (if (or (identp token)
          ;; If it's a curly brace, also print a space before it
          (blockp token)
          (and (not (separator-token-p token))
               (not (parenp token))
               (space-after-last-char-p buffer)))
    (buffer-write-char buffer #\Space))
  (buffer-write-string buffer
                       (token-text token))
  (when (or (blockp token)
            (and (eq (token-type token) :op)
                 (equal (token-text token) ";")))
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
