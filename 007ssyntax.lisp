;; Simple syntax shortcuts that expand to s-expressions.

(defun def-ssyntax(char handler-name &optional (precedence 0))
  (setf (gethash char *wart-ssyntax-precedence*)
        precedence)
  (setf (gethash char *wart-ssyntax-handler*)
        handler-name))



;; Internals

(defvar *wart-ssyntax-handler* (table))
(defvar *wart-ssyntax-precedence* (table))

(defun ssyntaxp(x)
  (and (symbolp x)
       (ssyntax-idx (symbol-name x))))

(defun expand-ssyntax(sym)
  (let ((symname (symbol-name sym)))
    (if (> (len symname) 1)
      (wt-transform (expand-ssyntax-string symname))
      sym)))

(add-wart-transformer ssyntaxp expand-ssyntax)

(defun expand-ssyntax-string(s)
  (let ((ssyntax-idx  (ssyntax-idx s)))
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

(defun ssyntax-idx(s &optional (chars (ssyntax-chars-in-precedence)))
  (and chars
       (or (rpos s (car chars))
           (ssyntax-idx s (cdr chars)))))

; decompose lowest precedence first
(defun ssyntax-chars-in-precedence()
  (sort (ssyntax-chars)
        (lambda(a b)
          (< (gethash a *wart-ssyntax-precedence*)
             (gethash b *wart-ssyntax-precedence*)))))

(defun ssyntax-chars()
  (let ((ans ()))
    (maphash (lambda(k v) (push k ans))
             *wart-ssyntax-precedence*)
    ans))

(defun ssyntax-parse-string(s)
  (if (all-digits s)
    (parse-integer s)
    (intern s)))

(defun all-digits(s)
  (no (position-if
        (lambda(x) (no (digit-char-p x)))
        s)))

; todo: idiomatic CL
(defun rpos(s c &optional (idx (1- (len s))))
  (if (>= idx 0)
    (if (eq c (char s idx))
      idx
      (rpos s c (1- idx)))))
