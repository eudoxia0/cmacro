(in-package :cl-user)
(defpackage cmacro-test
  (:use :cl :fiveam))
(in-package :cmacro-test)

(defparameter +cmacro-path+
  (asdf:component-pathname (asdf:find-system :cmacro-test)))

(defparameter +sample-file+
  (merge-pathnames
   #p"t/test.c"
   +cmacro-path+))

(def-suite tests
  :description "General tests.")
(in-suite tests)

(run! 'tests)
