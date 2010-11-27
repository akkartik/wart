;; Simple syntax shortcuts that expand to s-expressions.

; [..] => (lambda(_) (..))
; http://arclanguage.org/item?id=11551
(set-macro-character #\] (get-macro-character #\)))
(set-macro-character #\[
  #'(lambda(stream char)
      (declare (ignore char))
      (apply #'(lambda(&rest args)
                 `(fn(_) (,@args)))
             (read-delimited-list #\] stream t))))

(defun def-ssyntax(char handler-name)
  (setf (gethash char *ssyntax-handler*)
        handler-name))



;; Internals

(defvar *ssyntax-handler* (make-hash-table))

(defun ssyntaxp(x)
  (and (symbolp x)
       (ssyntax-char (symbol-name x))))

(defun expand-ssyntax(sym)
  (let ((symname (symbol-name sym)))
    (if (> (length symname) 1)
      (expand-ssyntax-string symname)
      sym)))

(defun expand-ssyntax-string(s &optional (idx (1- (length s)))) ; left-associative
  (let ((ssyntax-idx  (ssyntax-char s idx)))
    (if ssyntax-idx
      (let* ((ssyntax-char  (char s ssyntax-idx))
             (handler       (gethash ssyntax-char *ssyntax-handler*)))
        (if (is 0 ssyntax-idx)
          (list handler (intern (cut s 1))) ; unary op
          (list handler
                (expand-ssyntax-string (cut s 0 ssyntax-idx))
                (intern (cut s (1+ ssyntax-idx))))))
      (intern s))))

(defun ssyntax-char(s &optional (idx (1- (length s))))
  (and (>= idx 0)
       (or (if (position (char s idx)
                         '(#\~ #\! #\@ #\$ #\% #\^ #\& #\. #\+ #\_ #\< #\>))
             idx)
           (ssyntax-char s (1- idx)))))

(def-ssyntax #\^ 'compose)
(def-ssyntax #\~ 'complement)
(def-ssyntax #\. 'call)
(def-ssyntax #\! 'call-quoted)
