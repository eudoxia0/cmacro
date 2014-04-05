(in-package :cl-user)
(defpackage cmacro.preprocess
  (:use :cl)
  (:import-from :split-sequence
                :split-sequence)
  (:export :process-data
           :process-pathname))
(in-package :cmacro.preprocess)


(defparameter +cc+ "gcc")

(defparameter +preproc-cmd+
  (format nil "~A -xc - -E"
          +cc+))

(defmacro with-command ((cmd input &key on-error) &rest body)
  `(multiple-value-bind (stdout stderr exit-code)
       (trivial-shell:shell-command ,cmd :input ,input)
     (when (not (eql exit-code 0))
       ,on-error)
     ,@body))

(defun preprocess (data)
  "Call the C preprocessor to handle includes and C macros."
  (with-command (+preproc-cmd+
                 data
                 :on-error (progn
                             (format t "An error occurred during preprocessing:~&")
                             (format t "~A" stderr)
                             (sb-ext:quit)))
    ;; Remove leftovers lines that start with a #
    (reduce #'(lambda (line next-line)
                (concatenate 'string line (string #\Newline) next-line))
            (remove-if #'(lambda (line)
                           (if (> (length line) 0)
                               (char= #\# (elt line 0))))
                       (split-sequence #\Newline stdout)))))

(defparameter +cmc-lexer-bin+
  (first
   (remove-if #'null
              (mapcar #'probe-file
                      (list #p"/usr/bin/cmc-lexer"
                            (merge-pathnames
                             #p"grammar/cmc-lexer"
                             (asdf:component-pathname (asdf:find-system :cmacro))))))))

(defun lex (data)
  (with-command (+cmc-lexer-bin+
                 data
                 :on-error
                 (progn
                   (format t "An error occurred during lexing:~&")
                   (format t "~A" stderr)
                   (sb-ext:quit)))
    stdout))

(defun process-data (data)
  (lex (preprocess data)))

(defun slurp-file (path)
  ;; Credit: http://www.ymeme.com/slurping-a-file-common-lisp-83.html
  (with-open-file (stream path)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

(defun process-pathname (pathname)
  (process-data (slurp-file pathname)))
