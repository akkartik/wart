;; Override lisp keywords

; In return for being able to override keywords you can't use keywords anywhere
; in the program. Not even inside quoted expressions. Otherwise we can't
; handle backquoted expressions.

; intentionally ugly name; minimize its use
(defmacro defover(name new-name)
  `(progn
     (setf (gethash ',name *wart-special-form-handlers*)
           [cons ',new-name (cdr _)])
     (setf (gethash ',name *wart-special-form-quoted-handlers*)
           ',new-name)))



;; Internals

(defun lookup-unquoted-handler(sexp)
  (and (consp sexp)
       (gethash (car sexp) *wart-special-form-handlers*)))

(defun lookup-quoted-handler(name)
  (or (gethash name *wart-special-form-quoted-handlers*)
      name))

; handlers are functions of the input s-expr
(defvar *wart-special-form-handlers* (table))
; quoted handlers are names of the handlers; all handlers must be named
(defvar *wart-special-form-quoted-handlers* (table))

(add-wart-transformer
  [and (pairp _)
       (is 'quote (car _))]
  [list 'quote (lookup-quoted-handler (cadr _))])

(add-wart-transformer
  lookup-unquoted-handler
  [funcall (lookup-unquoted-handler _) _])
