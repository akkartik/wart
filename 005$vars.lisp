;; Implicit gensyms: http://letoverlambda.com/index.cl/guest/chap3.html#sec_5
;; Wart uses ! for ssyntax like arc, so instead of g! we use $ to denote gensyms.

(defun dollar-symbol-p(s)
  (and (symbolp s)
       (> (len (symbol-name s)) 1)
       (string= (symbol-name s) "$"
                :start1 0 :end1 1)))

(defmacro defmacro$(name args &rest body)
  (let ((syms (remove-duplicates
                (remove-if-not #'dollar-symbol-p
                               (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar (lambda(_)
                       `(,_   (uniq ,(cut (symbol-name _) 1))))
                     syms)
         ,@body))))

; Use defun$ to generate expressions for defmacro$ to return
; Identical to defmacro/$ except we replace defmacro in the body with defun
(defmacro defun$(name args &rest body)
  (let ((syms (remove-duplicates
                (remove-if-not #'dollar-symbol-p
                               (flatten body)))))
    `(defun ,name ,args
       (let ,(mapcar (lambda(_)
                       `(,_   (uniq ,(cut (symbol-name _) 1))))
                     syms)
         ,@body))))
