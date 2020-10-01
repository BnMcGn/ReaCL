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

(defpsmacro prop (&rest params)
  `(chain this #:props ,@params))

(defpsmacro props ()
  (@ this props))

(defpsmacro state (&rest params)
  `(chain this #:state ,@params))

(defpsmacro set-state (&rest params)
  `(chain this (#:set-state (create ,@params))))

(defpsmacro def-component (name &body body)
  `(defclass6 (,name (@l :reacl -react -component))
     ,@body))

(defpsmacro def-pure-component (name &body body)
    `(defclass6 (,name (react -pure-component))
       ,@body))



