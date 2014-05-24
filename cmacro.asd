(asdf:defsystem cmacro
  :version "0.2"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:split-sequence
               :trivial-types
               :anaphora
               :esrap
               :yason)
  :components ((:module "src"
                :serial t
                :components
                ((:file "error")
                 (:file "token")
                 (:file "macro")
                 (:file "parser")
                 (:file "pattern")
                 (:file "db")
                 (:file "printer")
                 (:file "template")
                 (:file "macroexpand")
                 (:file "cmacro"))))
  :description "Lisp macros for C"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cmacro-test))))
