(in-package :cl-user)
(defpackage cmacro-test-asd
  (:use :cl :asdf))
(in-package :cmacro-test-asd)

(defsystem cmacro-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cmacro
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "cmacro"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
