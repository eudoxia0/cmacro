(in-package :cl-user)
(defpackage cmacro
  (:use :cl :anaphora)
  (:import-from :cmacro.macroexpand
                :macroexpand-pathname)
  (:import-from :cmacro.printer
                :print-ast)
  (:export :main))
(in-package :cmacro)

(defun quit ()
  (sb-ext:exit :code -1))

(setf *debugger-hook* #'(lambda (c h)
                          (declare (ignore h))
                          (format t "~A~&" c)
                          (quit)))

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

(defun files (args binary-options)
  (flet ((optp (option)
           (and (>= (length option) 1)
                (char= (elt option 0) #\-))))
    (remove-if #'null
               (loop for sub-args on args collecting
                 (if (optp (first sub-args))
                     ;; Skip
                     (progn
                       (unless (member (first sub-args)
                                       binary-options
                                       :test #'equal)
                         (setf sub-args (rest sub-args)))
                       nil)
                     ;; It's a file
                     (first sub-args))))))

(defun process-file (pathname)
  (print-ast (macroexpand-pathname pathname)))

(defparameter +help+
"Usage: cmc [file]* [option]*

  -o, --output    Path to the output file
  -n,--no-expand  Don't macroexpand, but remove macro definitions
  -h,--help       Print this text")

(defun main (args)
  (let ((files       (mapcar #'parse-namestring
                             (files (cdr args)
                                    (list "-n" "--no-expand"))))
        (output-file (get-opt-value args "-o" "--output"))
        (helpp       (get-binary-opt args "-h" "--help")))
    (when helpp
      (format t "~A~%" +help+)
      (quit))
    (unless files
      (error 'cmacro.error:no-input-files))
    (if output-file
        ;; Write to a file
        (with-open-file (stream
                         output-file
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
          (loop for file in files do
            (write-string (process-file file)
                          stream)))
        ;; Write to stdout
        (progn
          (loop for file in files do
            (write-string (process-file file)))))
    (terpri)))
