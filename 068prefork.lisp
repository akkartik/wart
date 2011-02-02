;; http://common-lisp.net/pipermail/small-cl-src/2004-June/000064.html

(def start-server()
  (w/socket s 1978
    (repeat 2
      (fork (thunk
              (handle-request s))))
    (sb-posix:wait)))

;? (def handle-request(s)
;?   (loop for socket = (socket-accept s)
;?         for stream = (socket-make-stream socket :input t :output t)
;?         for client-id = (read-line stream)
;?         do (prn "received " client-id)
;?            (format stream "Hello ~A, this is PID ~D~%" client-id (sb-posix:getpid))
;?            (flush stream)
;?            (socket-close socket)))

;? (def handle-request(s)
;?   (loop
;?     (let* ((socket  (socket-accept s))
;?            (stream  (socket-make-stream socket :input t :output t))
;?            (client-id   (read-line stream)))
;?       (prn "received " client-id)
;?       (format stream "Hello ~A, this is PID ~D~%" client-id (sb-posix:getpid))
;?       (flush stream)
;?       (socket-close socket))))

(def handle-request(s)
  (loop
    (w/connection stream s
      (format stream "Hello ~A, this is PID ~D~%"
              (read-line stream) (sb-posix:getpid))
      (flush stream)
      (socket-close socket))))

;? (def handle-request(s)
;?   (w/connection stream s
;?      (format stream "hello ~a, this is pid ~d~%"
;?              read-line.stream
;?              (sb-posix:getpid))
;?      (flush stream)))
