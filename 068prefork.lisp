;; http://common-lisp.net/pipermail/small-cl-src/2004-June/000064.html

(require :sb-bsd-sockets)

(defconstant +nchildren+ 2)
(defconstant +backlog+ 16)

(defvar *server-socket* nil)

(def start-server()
  (setf *server-socket* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
  (wart-set (sb-bsd-sockets:sockopt-reuse-address *server-socket*))
  (sb-bsd-sockets:socket-bind *server-socket* (sb-bsd-sockets:make-inet-address "127.0.0.1") 1978)
  (sb-bsd-sockets:socket-listen *server-socket* +backlog+)
  (repeat +nchildren+
    (fork 'child-main))
  (sb-posix:wait))

(def child-main()
  (loop for socket = (sb-bsd-sockets:socket-accept *server-socket*)
        for stream = (sb-bsd-sockets:socket-make-stream socket :input t :output t)
        for client-id = (read-line stream)
        do (prn "received " client-id)
           (format stream "Hello ~A, this is PID ~D~%" client-id (sb-posix:getpid))
           (flush stream)
           (sb-bsd-sockets:socket-close socket)))
