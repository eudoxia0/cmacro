(in-package :cl-user)
(defpackage cmacro
  (:use :cl :anaphora)
  (:export :main))
(in-package :cmacro)

(defun extract-and-macroexpand (data)
  (destructuring-bind (ast macros) 
      (cmacro.macro:extract-macro-definitions (cmacro.parse:parse-data data))
    (cmacro.macro:macroexpand-ast ast macros)))

(defun macroexpand-data (data)
  (cmacro.parse:print-ast (extract-and-macroexpand data)))

(defun macroexpand-pathname (pathname)
  (macroexpand-data (cmacro.preprocess::slurp-file pathname)))

(defun get-opt (args boolean options)
  (first
   (remove-if #'null
              (mapcar #'(lambda (opt)
                          (aif (member opt args :test #'equal)
                               (if boolean
                                   (first it)
                                   (second it))))
                      options))))

(defun get-binary-opt (args &rest options)
  (get-opt args t options))

(defun get-opt-value (args &rest options)
  (get-opt args nil options))

(defun files (args)
  (flet ((optp (option)
           (and (>= (length option) 1)
                (char= (elt option 0) #\-))))
    (remove-if #'null
               (loop for sub-args on args collecting
                 (if (optp (first sub-args))
                     ;; Skip
                     (progn
                       (setf sub-args (rest sub-args))
                       nil)
                     ;; It's a file
                     (first sub-args))))))

(defparameter +help+ 
"Usage: cmc [file]* [option]*

  -o, --output    Path to the output file
  --dump-json     Dump a JSON of the AST
  -l,--lex        Dump the tokens (Without macroexpanding)
  -n,--no-expand  Don't macroexpand, but remove macro definitions")

(defun process-file (pathname dump-json-p lexp no-expand-p)
  (format t "Processing ~A~&" pathname)
  "placeholder")

(defun main (args)
  (let ((files       (mapcar #'parse-namestring (files (cdr args))))
        (output-file (get-opt-value args "-o" "--output"))
        (dump-json-p (get-binary-opt args "--dump-json"))
        (lexp        (get-binary-opt args "-l" "--lex"))
        (no-expand-p (get-binary-opt args "-n" "--no-expand")))
    (unless files
      (error 'cmacro.error:no-input-files))
    (loop for file in files do
      (let ((output (process-file file dump-json-p lexp no-expand-p)))
        (if output-file
            ;; Write to a file
            (with-open-file (stream
                             (parse-namestring output-file)
                             :direction :output
                             :if-does-not-exist :create)
              (write-string output stream))
            ;; Write to stdosut
            (progn
              (print output)
              (terpri)))))))
