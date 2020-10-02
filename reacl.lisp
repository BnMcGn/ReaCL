;;;; reacl.lisp

(in-package #:reacl)

(def-ps-package reacl
  :ps-requirements '(#:paren6)
  :js-requirements '(("react" "16.12.0"))
  :init-code
  (ps:ps
    (import-into (@l :reacl) ((:all -react)) "react")))

(defpsmacro psx (form)
  (compile-psx form))

(defpsmacro react (fname &rest params)
  `(chainl :reacl -react (,fname ,@params)))



