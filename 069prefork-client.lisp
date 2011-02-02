;; http://common-lisp.net/pipermail/small-cl-src/2004-June/000065.html

(defvar *socket* nil)
(defvar *stream* nil)

(def client(? object nil)
  "Return the echo from the server as multiple values."
  (setf *socket* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
  (sb-bsd-sockets:socket-connect *socket* (sb-bsd-sockets:make-inet-address "127.0.0.1") 1978)
  (setf *stream* (sb-bsd-sockets:socket-make-stream *socket* :input t :output t))

  (after
    (format *stream* "PID ~D~%" (sb-posix:getpid))
    (flush *stream*)

    (read-line *stream*)
  :do
    (sb-bsd-sockets:socket-close *socket*)))
