(defun wart-load(file)
  (let* ((len (length file))
         (ext (subseq file (- len 4))))
    (cond
      ((equal ext "lisp") (load file))
      ((equal ext "wart") (wt-load file)))))

(loop for path in (directory "./*.*") do
  (let ((file (file-namestring path)))
    (when (and (string< "" file)
               (char<= #\0 (char file 0))
               (char>= #\9 (char file 0)))
      (wart-load file))))

(unless *batch-mode*
  (wrepl))
