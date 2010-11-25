;; Simple transformer that allows us to override lisp keywords.

(defun wrepl()
  (loop
    (format t "w> ")(finish-output)
    (format t "~a~%" (wc-eval (read)))))

; To add a special form, write it as a macro with a different name, then
; register the macro with the right name.

(defmacro special-form(name new-name)
  `(progn
     (setf (gethash ',name *wc-special-form-handlers*)
           (lambda(_)
             (cons ',new-name (cdr _))))
     (setf (gethash ',name *wc-special-form-quoted-handlers*)
           ',new-name)))



;; Internals

(defun wc-eval(sexp)
  (eval (wc sexp)))

(defun wc(sexp)
  (apply-to-all-subtrees #'wc-1 sexp))

(defun wc-1(sexp)
  (cond
    ((no sexp)  sexp)
    ((atom sexp)  sexp)
    ((and (is 'quote (car sexp))
          (no (caddr sexp)))
                                      (list 'quote (lookup-quoted-handler (cadr sexp))))
    ((lookup-unquoted-handler sexp)   (call (lookup-unquoted-handler sexp) sexp))
    (t sexp)))

(defun lookup-unquoted-handler(sexp)
  (or (gethash (car sexp) *wc-special-form-handlers*)
      (gethash (type-of (car sexp)) *wc-type-handlers*)))

; In return for being able to override keywords you can't use keywords anywhere
; in the program. Not even inside quoted expressions.
(defun lookup-quoted-handler(name)
  (or (gethash name *wc-special-form-quoted-handlers*)
      (gethash (type-of name) *wc-type-quoted-handlers*)
      name))

; handlers are functions of the input s-expr
(defvar *wc-special-form-handlers* (make-hash-table))
(defvar *wc-type-handlers* (make-hash-table))
; quoted handlers are names of the handlers; all handlers must be named
(defvar *wc-special-form-quoted-handlers* (make-hash-table))
(defvar *wc-type-quoted-handlers* (make-hash-table))

(defun apply-to-all-subtrees(f sexp)
  (if (consp sexp)
    (call f (apply-to-all-leaves (lambda(_) (apply-to-all-subtrees f _)) sexp))
    sexp))

(defun apply-to-all-leaves(f sexp)
  (cond
    ((no sexp)  nil)
    ((atom sexp)  (call f sexp))
    ; consp sexp
    (t   (cons (call f (car sexp))
               (apply-to-all-leaves f (cdr sexp))))))

(defun function-name(f)
  (and (atom f)
       (ignore-errors (eval `(functionp ,f)))))

(defun function-form(s)
  (and (consp s)
       (find (car s) '(fn lambda))))
