(in-package :cl-user)
(defpackage cmacro-test
  (:use :cl :fiveam))
(in-package :cmacro-test)

(defparameter +sample-file+
  (merge-pathnames
   #p"t/test.c"
   (asdf:component-pathname (asdf:find-system :cmacro-test))))

(def-suite tests
  :description "General tests.")
(in-suite tests)

(test lex
  (is (list "int:10")
      (cmacro.preprocess:process-data "10"))
  (is (list "flt:2.2")
      (cmacro.preprocess:process-data "2.2"))
  (is (list "idn:derp")
      (cmacro.preprocess:process-data "derp"))
  (is (list "int:1" "opr:+" "int:1")
      (cmacro.preprocess:process-data "1 + 1"))
  (finishes
    (cmacro.preprocess:process-pathname +sample-file+)))

;; Parsing

(test parse
  (is (list (cmacro.parse:make-token :type :int :text "10"))
      (cmacro.parse:parse-data "10"))
  (is (list (cmacro.parse:make-token :type :op :text "{")
            (cmacro.parse:make-token :type :int :text "10"))
      (cmacro.parse:parse-data "{ 10 }")))

;; Macro definition

(test macro-case
  (is
    (cmacro.macro::parse-case
     (cmacro.parse:parse-data
      "match {
         nil
       }
       template {
         1 2 3;
       }"))
    (list :matching (list (list (cmacro.parse:make-token :type :ident
                                                         :text "nil")))
          :template "1 2 3 ; "
          :toplevel nil
          :external nil)))

(run! 'tests)
