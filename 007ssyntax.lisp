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
    (case ssyntax-idx
      ((nil)  (ssyntax-parse-token s))
      (0    (list (gethash (char s 0) *wart-ssyntax-handler*)
                  (expand-ssyntax-string (cut s 1)))) ; unary
      (t    (list (gethash (char s ssyntax-idx) *wart-ssyntax-handler*)
                  (expand-ssyntax-string (cut s 0 ssyntax-idx))
                  (expand-ssyntax-string (cut s (1+ ssyntax-idx))))))))

(defun ssyntax-idx(s &optional (chars (ssyntax-chars-in-precedence)))
  (and chars
       (or (position-if [position _ (car chars)] s
                        :from-end t) ; left-associative by default
           (ssyntax-idx s (cdr chars)))))

; decompose lowest precedence first
(defun ssyntax-chars-in-precedence()
  (group-by [gethash _ *wart-ssyntax-precedence*]
            (sort (ssyntax-chars) '<
                  :key [gethash _ *wart-ssyntax-precedence*])))

(defun ssyntax-chars()
  (let ((ans ()))
    (maphash (lambda(k v) (push k ans))
             *wart-ssyntax-precedence*)
    ans))

(defun ssyntax-parse-token(s)
  (if (all-digits s)
    (parse-integer s)
    (intern s)))

(defun all-digits(s)
  (no (position-if
        (lambda(x) (no (digit-char-p x)))
        s)))
