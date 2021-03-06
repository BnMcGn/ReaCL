;;;; def-component.lisp

(in-package #:reacl)

(defun needs-bindings-p (code)
  "Tries to determine if the supplied code needs access to 'props' or 'this'."
  ;; We are a little careless about where these symbols are placed in the code
  ;; because a false positive will do no harm.
  (and (member (first code) '(defun get set))
       (some (alexandria:rcurry #'member '(prop props state set-state thisref propsref))
             (alexandria:flatten code))))

(defun add-standard-bindings (func-code)
  `(,(car func-code)
    ,(second func-code)
    ,(third func-code)
    (var thisref (chain (lambda () this) (bind this)))
    (var propsref (chain (lambda () (@ this props)) (bind this)))
    ,@(cdddr func-code)))

(defun add-constructor-wrapper (const-code bind-clauses)
  `(defun constructor (props)
     (super props)
     (var thisref (chain (lambda () this) (bind this)))
     (var propsref (lambda () props))
     (macrolet ((set-state (&rest params)
                  `(setf (@ (thisref) state) (create ,@params))))
       ,@bind-clauses
       ,@const-code
       this)))

(defun find-bindables (body)
  "Collect function names to be bound in the constructor"
  (cl-utilities:collecting
      (dolist (form body)
        ;;For now we only collect defun clauses. Should we also do get/set?
        (when (eq 'defun (car form))
          (when (eq 'constructor (second form))
            ;; At this point we don't need bindables if a constructor is manually defined.
            (return-from find-bindables nil))
          (cl-utilities:collect (second form))))))

(defun bind-clauses (syms)
  (cl-utilities:collecting
    (dolist (itm syms)
      (unless (symbolp itm)
        (error "Function name must be a symbol"))
      (cl-utilities:collect `(setf (@ this ,itm) (chain this ,itm (bind this)))))))

(defun default-constructor (bindables)
  `(defun constructor (props)
     (super props)
     ,@bindables
     this))

(defun proc-component-body (constructor body)
  (let* ((bindables (find-bindables body))
         (con (when constructor (add-constructor-wrapper constructor (bind-clauses bindables))))
         (user-constructor nil)
         (res nil))
    (dolist (form body)
      (when (string-equal (second form) 'constructor)
        (if con
            (error "Constructor already defined as first parameter")
            (setf user-constructor t)))
      (push (if (needs-bindings-p form)
                (add-standard-bindings form)
                form)
            res))
    (unless (or con user-constructor)
      (setf con (default-constructor (bind-clauses bindables))))
    (when con
      (push con res))
    (nreverse res)))

;;FIXME: do we need to set :display-name with es6 react classes?
(defpsmacro def-component (name constructor &body body)
  `(defclass6 (,name (@l :reacl -react -component))
       ,@(proc-component-body constructor body)))

(defpsmacro def-pure-component (name constructor &body body)
  `(defclass6 (,name (@l :reacl -react -pure-component))
       ,@(proc-component-body constructor body)))

(defpsmacro prop (&rest params)
  `(chain (propsref) ,@params))

(defpsmacro state (&rest params)
  `(chain (thisref) #:state ,@params))

(defpsmacro set-state (&rest params)
  `(chain (thisref) (#:set-state (create ,@params))))
