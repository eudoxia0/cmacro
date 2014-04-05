(defparameter +cc+ "gcc")

(defparameter +cmc-lexer-bin+ "/usr/bin/cmc-lexer")

(defparameter +preproc-cmd+
  (format nil "~A -xc - -E | ~A"
          +cc+ +cmc-lexer-bin+))
