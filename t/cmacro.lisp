(in-package :cl-user)
(defpackage cmacro-test
  (:use :cl :fiveam))
(in-package :cmacro-test)

(defparameter +sample-file+
  (merge-pathnames
   #p"t/test.c"
   (asdf:component-pathname (asdf:find-system :cmacro-test))))

(def-suite preprocessing
  :description "Calling the lexer.")
(in-suite preprocessing)

(test lex
  (finishes
    (cmacro.preprocess:process-pathname +sample-file+)))

(run! 'preprocessing)
