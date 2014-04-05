(in-package :cl-user)
(defpackage cmacro-asd
  (:use :cl :asdf))
(in-package :cmacro-asd)

(defsystem cmacro
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :defsystem-depends-on (:asdf-linguist)
  :depends-on (:trivial-shell
               :cl-emb)
  :components ((:module "grammar"
                :components
                ((:flex "lexing" :output "lexing.yy")
                 (:c->bin "lexing.yy" :output "cmc-lexer" :link ("fl"))))
               (:module "src"
                :serial t
                :components
                ((:file "cmacro"))))
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
