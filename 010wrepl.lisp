;; Extensible wart transformer
;; Inspired by http://awwx.posterous.com/how-to-future-proof-your-code

; read transform macroexpand eval print
(defun wrepl()
  (loop
    (wt-prompt)
    (format t "~s~%" (wt-eval (read)))))

(defun wt-load(file)
  (with-open-file (f (merge-pathnames file))
    (loop with form = (read f)
          and eof = (gensym)
      do
        (wt-eval form)
        (setq form (read f nil eof))
      until (iso form eof))))

; Insert a case into the wart cond.
(defmacro add-wart-transformer(check trans)
  `(ignore-redef
      (setf *wart-cases* (append *wart-cases* (list (list (function ,check)
                                                          (function ,trans)))))
      (build-wart-transform)))



;; Internals

(defun wt-prompt()
  (unless *batch-mode*
    (format t "wart> ")(finish-output)))

(unless (boundp '*batch-mode*)
  (setf *batch-mode* nil))

(defun wt-eval(sexp)
  (eval (wt-transform sexp)))

(defun wt-transform(sexp)
  (apply-to-all 'wt-transform-1 sexp))

; code-generate wt-transform-1
(defun build-wart-transform()
  (eval (generate-wart-transform *wart-cases*)))

(defvar *wart-cases* ())

(defun generate-wart-transform(cases)
  `(defun wt-transform-1(sexp)
     (cond
       ((no sexp)   nil)
       ,@(convert-to-funcalls cases 'sexp)
       (t sexp))))

(defun convert-to-funcalls(cases var)
  (map 'list (lambda(case) (convert-to-funcall case var)) cases))

(defun convert-to-funcall(case var)
  (destructuring-bind (check trans) case
    `((funcall ,check ,var) (funcall ,trans ,var))))

(defun apply-to-all(f sexp)
  (cond
    ((no sexp)  nil)
    ((atom sexp)  (funcall f sexp))
    (t   (funcall f
                  (cons (apply-to-all f (car sexp))
                        (apply-to-all f (cdr sexp)))))))
