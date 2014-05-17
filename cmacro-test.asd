(asdf:defsystem cmacro-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cmacro
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "cmacro")))))
