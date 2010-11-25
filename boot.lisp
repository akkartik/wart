(loop for path in (directory "./*.*") do
  (let ((file (file-namestring path)))
    (when (and (string< "" file)
               (char<= #\0 (char file 0))
               (char>= #\9 (char file 0)))
      (let* ((len (length file))
             (ext (subseq file (- len 4))))
        (cond
          ((equal ext "lisp") (load file))
          ((equal ext "wart") (wload file)))))))

(wrepl)
