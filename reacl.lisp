;;;; reacl.lisp

(in-package #:reacl)

(def-ps-package #:reacl
  :ps-imports '(#:paren6)
  :)

(defpsmacro psx (form)
  (compile-psx form))

(defpsmacro react (function &rest params)
  `(chain -react ,function ,@params))

(defpsmacro rdom (function &rest params)
  `(chain -react -d-o-m ,function ,@params))

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

(defpsmacro def-pure-component
    `(defclass6 (,name (react -pure-component))
       ,@body))



