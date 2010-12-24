; [..] => (lambda(_) (..))
; http://arclanguage.org/item?id=11551
(set-macro-character #\] (get-macro-character #\)))
(set-macro-character #\[
  (lambda(stream char)
    (declare (ignore char))
    (apply (lambda(&rest args)
             `(lambda(_) (,@args)))
           (read-delimited-list #\] stream t))))
