;;;; prefork-client.lisp
;;;; http://common-lisp.net/pipermail/small-cl-src/2004-June/000065.html
;;;;
;;;; A very simple SBCL network client for use in testing the servers in
;;;; prefork-example-simple.lisp and prefork-example-realistic.lisp.

(in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :sb-posix)
  (require :sb-bsd-sockets)
  (use-package :sb-bsd-sockets))

;;; The client is broken across many functions in order to make it easier to
;;; poke at the server.
;;;
;;; We send a line identifying ourselves, then an s-expression.  The server
;;; identifies itself, then sends our s-expression back to us.

(defvar *socket* nil)
(defvar *stream* nil)

(defun client (&optional object)
  "Return the echo from the server as multiple values."
  (client-connect)
  (client-herald)
  (client-write (with-output-to-string (s) (print object s)))
  (multiple-value-prog1 (client-results)
    (socket-close *socket*)))

;;;
;;; Misc utils
;;; 

(defstruct (queue (:constructor %make-queue (list tail)))
  (list nil :type list)
  (tail nil :type list))

(defun queue (&rest elements)
  (make-queue elements))

(defun make-queue (&optional list)
  (%make-queue list (last list)))

(defun queue-add (item queue)
  (if (null (queue-tail queue))
      (setf (queue-list queue) (list item)
      (queue-tail queue) (queue-list queue))
      (setf (cdr (queue-tail queue)) (list item)
      (queue-tail queue) (cdr (queue-tail queue))))
  queue)

(defun queue-pop (queue)
  (prog1
      (pop (queue-list queue))
    (when (null (queue-list queue))
      (setf (queue-tail queue) nil))))

;;;
;;; Easily hang server processes
;;; 

(defvar *hung* (queue))

(defun hang (&optional (n 1))
  "Send N partial requests to the server, causing the processes to hang."
  (dotimes (i n)
    (let (*socket* *stream*)
      (client-connect)
      (client-herald)
      (client-write "(:hang ")
      (queue-add (cons *socket* *stream*) *hung*))))

(defun release (&optional (n 1))
  "Send the rest of a proper request to N hung servers."
  (loop repeat n
        for pair = (queue-pop *hung*)
        when pair collect (destructuring-bind (*socket* . *stream*) pair
          (client-write ":release) ")
          (client-results))))

;;;
;;; The guts
;;; 

(defun client-connect ()
  (setf *socket* (make-instance 'inet-socket :type :stream :protocol :tcp))
  (socket-connect *socket* (make-inet-address "127.0.0.1") 1978)
  (setf *stream* (socket-make-stream *socket* :input t :output t)))
  
(defun client-herald ()
  (format *stream* "PID ~D~%" (sb-posix:getpid))
  (finish-output *stream*))
      
(defun client-write (string)
  "Send STRING to the server.  If this is an incomplete sexp, it can be used to
hang the server."
  (write-string string *stream*)
  (finish-output *stream*))

(defun client-results ()
  (values (read-line *stream*)
    (read *stream*)))
