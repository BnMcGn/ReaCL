;;;; reacl.lisp

(in-package #:reacl)

(def-ps-package reacl
  ;; Version 17 is still too bleeding edge.
  :js-requirements '(("react" "16.0.0"))
  :init-code
  (ps:ps
    (import-into (@l :reacl) ((:all -react)) "react")))

(defpsmacro psx (form)
  (compile-psx form))

(defpsmacro react (fname &rest params)
  `(chainl :reacl -react (,fname ,@params)))

;; Context stuff

(defpsmacro defcontext (name value &key display-name)
  `(progn
     (var ,name (react create-context ,value))
     (setf (@ ,name display-name) ',(or display-name name))))

(defpsmacro let-context ((&rest context-pairs) &body body)
  (let ((accum `(list ,@body)))
    (dolist (itm (reverse context-pairs))
      (unless (listp itm)
        (error "Contexts must be provided in (context value) pairs"))
      (setf
       accum
       `(react create-element (@ ,(car itm) -provider)
               (create value ,(second itm))
               ,accum)))
    accum))

(defpsmacro get-context ((&rest context-bindings) &body body)
  (let ((accum `(list ,@body)))
    (dolist (itm (reverse context-bindings))
      (let ((context (if (listp itm) (car itm) itm))
            (binding (if (listp itm) (second itm) itm)))
        (setf
         accum
         `(react create-element (@ ,context -consumer)
                 (create :key 1)
                 (paren6:=> ,binding ,accum)))))
    accum))

