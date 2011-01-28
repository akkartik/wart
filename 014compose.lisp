(defun compose-fn(f g)
  (lambda(&rest args)
    (call-fn f (wart-apply g args))))

(defmacro compose(f g)
  `(compose-fn (fslot ,f) (fslot ,g)))
