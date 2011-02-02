;; http://common-lisp.net/pipermail/small-cl-src/2004-June/000065.html

(def client()
  (connecting stream :at 1978
    (format stream "PID ~D~%" (getpid))
    (flush stream)
    (read-line stream)))
