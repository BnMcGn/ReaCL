;;;; ReaCL.asd

(asdf:defsystem #:ReaCL
  :description "Describe ReaCL here"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:paren6 #:ps-lib-tool #:gadgets)
  :components ((:file "package")
               (:file "reacl")))
