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
  (is (equal (list "int:10")
             (cmacro.preprocess:process-data "10")))
  (is (equal (list "flt:2.2")
             (cmacro.preprocess:process-data "2.2")))
  (is (equal (list "idn:derp")
             (cmacro.preprocess:process-data "derp")))
  (is (equal (list "int:1" "opr:+" "int:1")
             (cmacro.preprocess:process-data "1 + 1")))
  (finishes
    (cmacro.preprocess:process-pathname +sample-file+)))

(run! 'preprocessing)
