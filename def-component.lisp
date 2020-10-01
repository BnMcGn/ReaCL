;;;; def-component.lisp

(in-package #:reacl)

(defpsmacro def-component (name &body body)
  `(defclass6 (,name (@l :reacl -react -component))
     ,@body))

(defpsmacro def-pure-component (name &body body)
    `(defclass6 (,name (react -pure-component))
       ,@body))



