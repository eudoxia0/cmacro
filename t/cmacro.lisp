(in-package :cl-user)
(defpackage cmacro-test
  (:use :cl :fiveam)
  (:import-from :esrap
                :parse)
  (:import-from :cmacro.token
                :<identifier>
                :<variable>
                :token-text
                :make-variable)
  (:import-from :cmacro.pattern
                :match-group
                :match-var
                :match-token))
(in-package :cmacro-test)

(defparameter +cmacro-path+
  (asdf:component-pathname (asdf:find-system :cmacro-test)))

(def-suite parser
  :description "Parsing tests.")
(in-suite parser)

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

(def-suite pattern-matcher
  :description "Pattern matching tests.")
(in-suite pattern-matcher)

(test match-tokens
  (is-true (match-var (make-variable "test")
                      nil))
  (is-true (match-var (make-variable "test")
                      (make-instance '<identifier> :text "1")))
  (is-true (match-var (make-variable "test")
                      (list 1 2 3))))

(test match-groups
  (is-true (match-group (make-variable "test list")
                        (list :list))))

(run! 'parser)
(run! 'pattern-matcher)
