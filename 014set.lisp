;; Extensible assignment

(defmacro wart=(&rest args)
  (let* ((pairs  (pair args))
         (vars  (map 'list 'car pairs))
         (vals  (map 'list 'cadr pairs))
         (uniqs  (loop repeat (len vals)
                       collect (uniq))))
    `(let ,(pair (interleave uniqs vals))
        (psetf ,@(interleave vars uniqs))
        ,(car (last uniqs)))))
(defover = wart=)

(defmacro =*(&rest args)
  `(setf ,@args))

(defmacro defset(type args &rest body)
  `(defcoerce ,type function=
    (lambda ,args
      ,@body)))



;; Internals

(defun call-setf(&rest args)
  (indexing wart-coercions* ('function=
                             (wart-type (car args)))
    (apply it args)))
(defsetf call call-setf)
