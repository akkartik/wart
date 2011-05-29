;; Guards for def and mac

(extend-macro def(name params &body body) :case (iso :case (car body))
  `(prog1 ',name
     (push (list (allf (length-matcher ',params)
                       (fn ,params ,(cadr body)))
                 (fn ,params
                   (handling-$vars ,@(cddr body))))
           (gethash ',name wart-signatures*))))

(extend-macro mac(name params &body body) :case (iso :case (car body))
  `(extend-macro ,name ,params :case ,(cadr body)
     ,@(cddr body)))



;; Internals

(defun allf(&rest tests)
  (if (no tests)
    (lambda(&rest args) t)
    (lambda(&rest args)
      (and (apply (car tests)
                  args)
           (apply (apply 'allf (cdr tests))
                  args)))))

