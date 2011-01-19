(synonym len length
         rev reverse
         cut subseq
         join append
         keep remove-if-not
         trunc truncate)

(defun singlep(x)
  (and (consp x)
       (no (cdr x))))

(defun pairp(x)
  (and (consp x)
       (consp (cdr x))
       (no (cddr x))))

(defun tuples(n xs &optional acc)
  (if (no xs)
    (nreverse acc)
    (tuples n (nthcdr n xs) (cons (firstn n xs) acc))))

(defun pair(xs)
  (tuples 2 xs))

(defun firstn(n xs)
  (if (or (= n 0) (no xs))
      nil
      (cons (car xs) (firstn (1- n) (cdr xs)))))

(defun flatten(tree)
  (let ((result '()))
    (labels ((scan (item)
               (if (listp item)
                 (map nil #'scan item)
                 (push item result))))
      (scan tree))
    (nreverse result)))

(defun alref(key alist)
  (cdr (assoc key alist)))

; helper for certain kinds of recursive functions
(defun append-or-afresh(f x xss)
  (if (funcall f x)
    (cons (cons x (car xss))
          (cdr xss))
    (cons (list x)
          xss)))

(defun group-by(f xs &optional acc)
  (if xs
    (group-by f (cdr xs)
              (append-or-afresh [is (funcall f _)
                                    (funcall f (car (car acc)))]
                                (car xs)
                                acc))
    (nreverse acc)))
