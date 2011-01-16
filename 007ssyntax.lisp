;; Simple syntax shortcuts that expand to s-expressions.

(defun def-ssyntax(char handler-name)
  (setf (gethash char *wart-ssyntax-handler*)
        handler-name))



;; Internals

(defvar *wart-ssyntax-handler* (table))

(defun ssyntaxp(x)
  (and (symbolp x)
       (ssyntax-char (symbol-name x))))

(defun expand-ssyntax(sym)
  (let ((symname (symbol-name sym)))
    (if (> (len symname) 1)
      (wt-transform (expand-ssyntax-string symname))
      sym)))

(add-wart-transformer ssyntaxp expand-ssyntax)

(defun expand-ssyntax-string(s &optional (idx (1- (len s)))) ; left-associative
  (let ((ssyntax-idx  (ssyntax-char s idx)))
    (if ssyntax-idx
      (let* ((ssyntax-char  (char s ssyntax-idx))
             (handler       (gethash ssyntax-char *wart-ssyntax-handler*)))
        (if (is 0 ssyntax-idx) ; unary
          (list handler
                (expand-ssyntax-string (cut s 1)))
          (list handler
                (expand-ssyntax-string (cut s 0 ssyntax-idx))
                (expand-ssyntax-string (cut s (1+ ssyntax-idx))))))
      (ssyntax-parse-string s))))

(defun ssyntax-char(s &optional (idx (1- (len s))))
  (and (>= idx 0)
       (or (if (position (char s idx)
                         '(#\~ #\! #\@ #\$ #\% #\^ #\. #\{ #\} #\[ #\])) ; & _ + < >
             idx)
           (ssyntax-char s (1- idx)))))

(defun ssyntax-parse-string(s)
  (if (all-digits s)
    (parse-integer s)
    (intern s)))

(defun all-digits(s)
  (no (position-if
        (lambda(x) (no (digit-char-p x)))
        s)))
