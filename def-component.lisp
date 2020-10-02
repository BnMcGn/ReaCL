;;;; def-component.lisp

(in-package #:reacl)

(defun needs-bindings-p (code)
  "Tries to determine if the supplied code needs access to 'props' or 'this'."
  ;; We are a little careless about where these symbols are placed in the code
  ;; because a false positive will do no harm.
  (and (member (first code) '(defun get set))
       (some (alexandria:rcurry #'member '(prop props state set-state thisref))
             (alexandria:flatten code))))

(defun add-standard-bindings (func-code)
  `(,(car func-code)
    ,(second func-code)
    ,(third func-code)
    (var thisref (chain (lambda () this) (bind this)))
    (var propsref (chain (lambda () (@ this props)) (bind this)))
    ,@(cdddr func-code)))

(defun add-constructor-wrapper (const-code)
  `(defun constructor (props)
     (super props)
     (var thisref (chain (lambda () this) (bind this)))
     (var propsref (lambda () props))
     (macrolet ((set-state (&rest params)
                  `(setf (@ (thisref) state) (create ,@params))))
       ,@const-code
       this)))

(defun proc-component-body (constructor body)
  (let ((con (when constructor (add-constructor-wrapper constructor)))
        (res nil))
    (dolist (form body)
      (when (and con (string-equal (second form) 'constructor))
        (error "Constructor already defined as first parameter"))
      (push (if (needs-bindings-p form)
                (add-standard-bindings form)
                form)
            res))
    (when con
      (push con res))
    (nreverse res)))

;;FIXME: do we need to set :display-name with es6 react classes?
(defpsmacro def-component (name constructor &body body)
  `(defclass6 (,name (@l :reacl -react -component))
       ,@(proc-component-body constructor body)))

(defpsmacro def-pure-component (name constructor &body body)
  `(defclass6 (,name (react -pure-component))
       ,@(proc-component-body constructor body)))

(defpsmacro prop (&rest params)
  `(chain (propsref) ,@params))

(defpsmacro state (&rest params)
  `(chain (thisref) #:state ,@params))

(defpsmacro set-state (&rest params)
  `(chain (thisref) (#:set-state (create ,@params))))
