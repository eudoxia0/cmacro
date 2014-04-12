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

;; Variable extraction

(defparameter +vars+
  "void f() {
     $(my-variable);
     $(another-var:int);
   }")

(test vars
  (finishes (cmacro.var:extract-vars (cmacro.parse:parse-data +vars+))))

;; Variable creation

(test var-creation
  (is (equalp (cmacro.var:make-var :name "my-var" :qualifiers nil)
              (cmacro.var::extract-var "my-var"))))

;; Variable matching

(test tok-match
  (is-false (cmacro.var:match-token
             (cmacro.parse:make-token :type :ident :text "derp")
             (cmacro.parse:make-token :type :op :text "")))
  (is-false (cmacro.var:match-token
             (cmacro.parse:make-token :type :integer :text "derp")
             (cmacro.parse:make-token :type :integer :text "herp")))
  (is-true (cmacro.var:match-token
             (cmacro.parse:make-token :type :integer :text "derp")
             (cmacro.parse:make-token :type :integer :text "derp")))
  (is-true (cmacro.var:match-token
             (cmacro.var:extract-var "my-var")
             (cmacro.parse:make-token :type :integer :text "10")))
  (is-true (cmacro.var:match-token
             (cmacro.var:extract-var "my-var:int")
             (cmacro.parse:make-token :type :integer :text "10")))
  (is-true (cmacro.var:match-token
             (cmacro.var:extract-var "my-var:float")
             (cmacro.parse:make-token :type :float :text "3.14")))
  (is-true (cmacro.var:match-token
             (cmacro.var:extract-var "my-var:string")
             (cmacro.parse:make-token :type :string :text "\"derp\""))))

;; Pattern matching

(test pat-match
  ;; Atoms
  (is-true (cmacro.var:match
             (cmacro.var:extract-var "my-var:int")
             (cmacro.parse:make-token :type :integer :text "10")))
  (is-true (cmacro.var:match
             (cmacro.var:extract-var "my-var:float")
             (cmacro.parse:make-token :type :float :text "3.14")))
  (is-true (cmacro.var:match
             (cmacro.var:extract-var "my-var:string")
             (cmacro.parse:make-token :type :string :text "\"derp\"")))
  ;; Lists
  (is
    (cmacro.var:match
      (list
       (cmacro.parse:make-token :type :integer :text "1")
       (cmacro.var:extract-var "my-var:int"))
      (list
       (cmacro.parse:make-token :type :integer :text "1")
       (cmacro.parse:make-token :type :integer :text "2")))
    (list t (list (make-var :name "my-var" :qualifiers (:integer))
                  (cmacro.parse:make-token :type :integer :text "2")))))

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
