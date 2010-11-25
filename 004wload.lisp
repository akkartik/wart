;; Load .wart files

(defun wload(file)
  (with-open-file (f (merge-pathnames file))
    (loop with form = (read f)
          and eof = (gensym)
      do
        (wc-eval form)
        (setq form (read f nil eof))
      until (is form eof))))
