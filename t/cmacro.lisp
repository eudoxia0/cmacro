(in-package :cl-user)
(defpackage cmacro-test
  (:use :cl :fiveam)
  (:import-from :esrap
                :parse)
  (:import-from :cmacro.token
                :token-text))
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

(test integer-parsing
  (is
   (equal "0123"
          (parse 'cmacro.parser::octal "0123")))
  (is
   (equal "0x123"
          (parse 'cmacro.parser::hex "0x123")))
  (is
   (equal "123"
          (parse 'cmacro.parser::dec "123")))
  (is (equal "123"
             (token-text (parse 'cmacro.parser::integer "123")))))

(test string-parsing
  (is
   (equal "\"test\""
          (token-text (parse 'cmacro.parser::string "\"test\""))))
  (is
   (equal "\"test \\\"inner string\\\"  test\""
          (token-text (parse 'cmacro.parser::string
                             "\"test \\\"inner string\\\"  test\"")))))

(test identifier-parsing
  (is
   (equal "test"
          (token-text (parse 'cmacro.parser::identifier "test"))))
  (is
   (equal "c_function01"
          (token-text (parse 'cmacro.parser::identifier "c_function01")))))

(run! 'tests)
