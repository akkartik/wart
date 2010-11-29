;; Simple syntax shortcuts that expand to s-expressions.

(defun def-ssyntax(char handler-name)
  (setf (gethash char *wart-ssyntax-handler*)
        handler-name))

(defvar *wart-ssyntax-handler* (table))

(def-ssyntax #\^ 'compose)
(def-ssyntax #\~ 'complement)
(def-ssyntax #\. 'call*)
(def-ssyntax #\! 'call*-quoted)



;; Internals

(defun ssyntaxp(x)
  (and (symbolp x)
       (ssyntax-char (symbol-name x))))

(defun expand-ssyntax(sym)
  (let ((symname (symbol-name sym)))
    (if (> (len symname) 1)
      (wt-transform (expand-ssyntax-string symname))
      sym)))

(add-wart-transformer #'ssyntaxp #'expand-ssyntax)

(defun expand-ssyntax-string(s &optional (idx (1- (len s)))) ; left-associative
  (let ((ssyntax-idx  (ssyntax-char s idx)))
    (if ssyntax-idx
      (let* ((ssyntax-char  (char s ssyntax-idx))
             (handler       (gethash ssyntax-char *wart-ssyntax-handler*)))
        (if (is 0 ssyntax-idx)
          (list handler (intern (cut s 1))) ; unary
          (list handler
                (expand-ssyntax-string (cut s 0 ssyntax-idx))
                (ssyntax-parse-string (cut s (1+ ssyntax-idx))))))
      (intern s))))

(defun ssyntax-char(s &optional (idx (1- (len s))))
  (and (>= idx 0)
       (or (if (position (char s idx)
                         '(#\~ #\! #\@ #\$ #\% #\^ #\. #\< #\>)) ; & _ +
             idx)
           (ssyntax-char s (1- idx)))))

(defun ssyntax-parse-string(s)
  (if (all-digits s)
    (parse-integer s)
    (intern s)))

(defun all-digits(s)
  (no (position-if (lambda(x) (no (digit-char-p x))) s)))

(defmacro call*(a b)
  (cond
    ((macp a)   `(,a ,b))
    ((function-name-p a)  `(call (function ,a) ,b))
    (t  `(call ',a ,b))))

(defmacro call*-quoted(a b)
  `(call* ,a ',b))

(defun function-name-p(f)
  (and (atom f)
       (eval `(functionp ',f))))
