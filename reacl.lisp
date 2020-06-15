;;;; reacl.lisp

(in-package #:reacl)

(def-ps-package reacl
  :ps-imports '(#:paren6)
  :js-imports '(("react" "16.12.0"))
  :code
  (ps:ps
    (import-into (@l :reacl) (-react) "react")))

(defpsmacro psx (form)
  (compile-psx form))

(defpsmacro react (fname &rest params)
  `(chainl -react ,fname ,@params))

(defpsmacro prop (&rest params)
  `(chain this #:props ,@params))

(defpsmacro props ()
  (@ this props))

(defpsmacro state (&rest params)
  `(chain this #:state ,@params))

(defpsmacro set-state (&rest params)
  `(chain this (#:set-state (create ,@params))))

(defpsmacro def-component (name &body body)
  `(defclass6 (,name (react -component))
     ,@body))

(defpsmacro def-pure-component (name &body body)
    `(defclass6 (,name (react -pure-component))
       ,@body))



