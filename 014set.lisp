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

(defmacro$ defset(type args &rest body)
  (if (not (and (gethash 'function= wart-coercions*)
                (gethash type (gethash 'function= wart-coercions*))))
  `(defcoerce ,type function=
    (lambda ,args
      ,@body))
  `(defcoerce ,type function=
     (let ((,$oldfn (gethash ',type (gethash 'function= wart-coercions*))))
       (lambda(&rest ,$args)
         (if (iso (len ',args) (len ,$args))
           (destructuring-bind ,args ,$args
             ,@body)
           (apply ,$oldfn ,$args)))))))



;; Internals

(defun call-setf(&rest args)
  (indexing wart-coercions* ('function=
                             (wart-type (car args)))
    (apply it args)))
(defsetf call call-setf)
