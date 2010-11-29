;; call uses extensible coerce.

(ignore-redef
  (defun call(f &rest args)
    (if (or (functionp f)
            (and (symbolp f) (fboundp f)))
      (apply f args)
      (apply (wart-coerce f 'function) args))))
