;; Simple syntax shortcuts that expand to s-expressions.

; [..] => (lambda(_) (..))
; http://arclanguage.org/item?id=11551
(set-macro-character #\] (get-macro-character #\)))
(set-macro-character #\[
  #'(lambda(stream char)
      (declare (ignore char))
      (apply #'(lambda(&rest args)
                 `(lambda(_) (,@args)))
             (read-delimited-list #\] stream t))))

(defun def-ssyntax(char handler-name)
  (setf (gethash char *wart-ssyntax-handler*)
        handler-name))



;; Internals

(defvar *wart-ssyntax-handler* (make-hash-table))

(defun ssyntaxp(x)
  (and (symbolp x)
       (ssyntax-char (symbol-name x))))

(defun expand-ssyntax(sym)
  (let ((symname (symbol-name sym)))
    (if (> (len symname) 1)
      (expand-ssyntax-string symname)
      sym)))

(defun expand-ssyntax-string(s &optional (idx (1- (len s)))) ; left-associative
  (let ((ssyntax-idx  (ssyntax-char s idx)))
    (if ssyntax-idx
      (let* ((ssyntax-char  (char s ssyntax-idx))
             (handler       (gethash ssyntax-char *wart-ssyntax-handler*)))
        (if (is 0 ssyntax-idx)
          (list handler (intern (cut s 1))) ; unary
          (list handler
                (expand-ssyntax-string (cut s 0 ssyntax-idx))
                (intern (cut s (1+ ssyntax-idx))))))
      (intern s))))

(defun ssyntax-char(s &optional (idx (1- (len s))))
  (and (>= idx 0)
       (or (if (position (char s idx)
                         '(#\~ #\! #\@ #\$ #\% #\^ #\. #\< #\>)) ; & _ +
             idx)
           (ssyntax-char s (1- idx)))))

(add-wart-transformer #'ssyntaxp #'expand-ssyntax)
(add-wart-transformer #'atom #'idfn)

(def-ssyntax #\^ 'compose)
(def-ssyntax #\~ 'complement)
(def-ssyntax #\. 'call)
(def-ssyntax #\! 'call-quoted)
