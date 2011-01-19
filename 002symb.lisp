;; http://www.paulgraham.com/onlisp.html, section 4.7

(defun mkstr(&rest args)
  (with-output-to-string(s)
    (dolist (a args)
      (princ a s))))

(defun symb(&rest args)
  (intern (apply 'mkstr args)))
