(defun compose(f g)
  (lambda(&rest args)
    (call-fn f (wart-apply g args))))

(defmacro compose*(f g)
  `(compose (fslot ,f) (fslot ,g)))
