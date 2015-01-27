(in-package :cl-user)
(defpackage cmacro-test
  (:use :cl :fiveam)
  (:import-from :esrap
                :parse)
  (:import-from :cmacro.token
                :<identifier>
                :<variable>
                :token-text
                :token-equal
                :make-variable)
  (:import-from :cmacro.parser
                :parse-string
                :parse-pathname)
  (:import-from :cmacro.pattern
                :match
                :<match>
                :match-bindings
                :equal-bindings)
  (:import-from :cmacro.template
                :render-template))
(in-package :cmacro-test)

(defparameter +cmacro-path+
  (asdf:component-pathname (asdf:find-system :cmacro-test)))

(def-suite parser
  :description "Parsing tests.")
(in-suite parser)

(test integer-parsing
  (is (equal "0123" (parse 'cmacro.parser::octal "0123")))
  (is (equal "0x123" (parse 'cmacro.parser::hex "0x123")))
  (is (equal "123" (parse 'cmacro.parser::dec "123")))
  (is (equal "123" (token-text (parse 'cmacro.parser::integer "123")))))

(test string-parsing
  (is (equal "\"test\"" (token-text (parse 'cmacro.parser::string "\"test\""))))
  (is (equal "\"test \\\"inner string\\\"  test\""
             (token-text (parse 'cmacro.parser::string
                                "\"test \\\"inner string\\\"  test\"")))))

(test identifier-parsing
  (is (equal "test"
             (token-text (parse 'cmacro.parser::identifier "test"))))
  (is (equal "c_function01"
             (token-text (parse 'cmacro.parser::identifier "c_function01")))))

(def-suite pattern-matcher
  :description "Pattern matching tests.")
(in-suite pattern-matcher)

(defun matches (pattern input bindings)
  (let ((match (match pattern input))
        (bindings (cmacro.pattern::bindings->hash-table bindings)))
    (if match
        (equal-bindings (getf match :bindings) bindings))))

(test match-atom
  (is-true
    (matches (make-variable "test")
             (make-instance '<identifier> :text "1")
             (list (list (make-variable "test") 1))))
  (is-true
    (matches (make-instance '<identifier> :text "1")
             (make-instance '<identifier> :text "1")
             (list)))
  (is-false
    (matches (make-instance '<identifier> :text "1")
             (make-instance '<identifier> :text "2")
             (list))))

(test match-expression
  (matches (parse-string "1 2 $(var)")
           (parse-string "1 2 3")
           (list (list (make-variable "test") 3))))

(def-suite templates
  :description "Pattern matching tests.")
(in-suite templates)

(test variable-replacement
  (is (equal
       (render-template (list (make-variable "test"))
                        (let ((table (make-hash-table :test #'equal)))
                          (setf (gethash "test" table)
                                1)
                          table))
       (list 1))))

(test gen-get-sym
  (is (token-equal
       (first (render-template (list (make-variable "@gensym label"))
                               (list)))
       (make-instance '<identifier>
                      :text "cmacro_label_1")))
  (is (token-equal
       (first (render-template (list (make-variable "@getsym label"))
                               (list)))
       (make-instance '<identifier>
                      :text "cmacro_label_1")))
  (finishes
    (setf cmacro.db::*symbol-index* 0)))

(run! 'parser)
(run! 'pattern-matcher)
(run! 'templates)
