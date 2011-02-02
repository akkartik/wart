;;;; prefork-example-simple.lisp
;;;; http://common-lisp.net/pipermail/small-cl-src/2004-June/000064.html
;;;;
;;;; This is a simple example of writing a preforking Unix server using SBCL.
;;;; This file was written to demonstrate the basic techniques of writing a
;;;; preforking server for educational purposes.  For a more nuanced example,
;;;; see prefork-example-realistic.lisp
;;;;
;;;; Because it uses fork(2) and waitpid(2), this file requires a
;;;; patched version of the sb-posix contrib module for SBCL.  A patch
;;;; against SBCL-0.8.8 is available separately.
;;;;
;;;; We also assumes a BSD-like system, due to its use of flock(2) for
;;;; serialization.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix)
  (require :sb-bsd-sockets))

(use-package :cl :sb-bsd-sockets)

;? (defpackage :prefork-example-simple
;?   (:use :cl :sb-bsd-sockets))
;? 
;? (in-package :prefork-example-simple)

;;; In this simple example, the parent sets up a server socket, forks
;;; +NCHILDREN+ child processes, then just sits there, and periodically reaps
;;; and reforks any child processes that die.
;;;
;;; The child processes wait for incoming connections.  Not all Unixes serialize
;;; `accept's in the kernel, so we need to handle this ourselves.  We do this by
;;; acquiring an exclusive file lock around the call to accept(2).  We use
;;; flock(2) to do this, but we could just as well use fcntl(2) or SysV
;;; semaphores.


;;;
;;; The Parent
;;; 

(defconstant +nchildren+ 128)
(defconstant +backlog+ 16)

(declaim (type (simple-array (or integer null)) *children*))
(defvar *children* (make-array +nchildren+ :initial-element nil)
  "the PIDs of our children")

(defvar *childp* nil "True in child processes.")

(defvar *server-socket* nil)
(defvar *lock* nil)

(defun start-server ()
  "The main loop of the parent process."
  (setup)
  (unwind-protect
       (progn (fork-children)
        (loop
     ;; We call SERVE-EVENT here to let other Lisp applications
     ;; run, eg, SLIME.
     (sb-sys:serve-event 60)
     ;; Periodically reap any dead processes.
     (reap-children)))
    (unless *childp* (stop-server))))

(defun stop-server ()
  (kill-children)
  (teardown-lock)
  (sb-bsd-sockets:socket-close *server-socket*))

(defun setup ()
  "Set up sockets and locks so the parent can begin forking."
  (setup-lock)
  (setup-parent-signal-handlers)
  (when *server-socket* (sb-bsd-sockets:socket-close *server-socket*))
  (setf *server-socket* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
  (setf (sb-bsd-sockets:sockopt-reuse-address *server-socket*) t)
  (sb-bsd-sockets:socket-bind *server-socket* (sb-bsd-sockets:make-inet-address "127.0.0.1") 1978)
  (sb-bsd-sockets:socket-listen *server-socket* +backlog+))


;;;
;;; The child
;;; 

(defun child-main ()
  "Get in the queue to accept, serve the request, loop."
  (setup-child-signal-handlers)
  (loop for socket = (sb-bsd-sockets:socket-accept *server-socket*)
        for stream = (sb-bsd-sockets:socket-make-stream socket :input t :output t)
        for client-id = (read-line stream)
        for message = (read stream)
        do (format stream "Hello ~A, this is PID ~D~%~S"
       client-id (sb-posix:getpid) message)
           (finish-output stream)
           (sb-bsd-sockets:socket-close socket)))

;;;
;;; Forking and reaping
;;; 

(defun fork-children ()
  (dotimes (i +nchildren+)
    (fork-one-child i)))

(defun fork-one-child (index)
  (labels ((child()
             (unwind-protect
                (progn
                  (setf *children* (vector)
                        *childp* t)
                  (child-main))
                 (sb-ext:quit)))
           (parent(pid)
             (setf (aref *children* index) nil)))
    (let ((pid (sb-posix:fork)))
      (if (zerop pid) (child) (parent pid)))))

(defun reap-children ()
  "Unix is sick."
  (when (every #'null *children*) (return-from reap-children))
  (loop for pid = (ignore-errors (sb-posix:waitpid 0 sb-posix::wnohang))
        while pid do
        (let ((index (position pid *children*)))
    (when index
      (warn "Reaping child process ~D" pid)
      (fork-one-child index)))))

(defun kill-children ()
  "Unix is sick."
  (unless *childp*
    (loop for pid across *children*
    for index upfrom 0
          when pid do (handler-case (sb-posix:kill pid sb-posix::sigterm)
      (sb-posix:syscall-error (error)
        (warn "Could not kill PID ~D: ~A" pid error)
        (setf (aref *children* index) nil))))
    (loop for pid across *children*
          for index upfrom 0
          when pid do (ignore-errors (sb-posix:waitpid pid 0))
                (setf (aref *children* index) nil))))

;;;
;;; Signals
;;; 

(defun setup-child-signal-handlers ()
  (sb-sys:enable-interrupt sb-unix:sigterm (signal-handler 'sb-ext:quit))
  (sb-sys:ignore-interrupt sb-unix:sigint))
   
(defun setup-parent-signal-handlers ()
  (sb-sys:enable-interrupt sb-unix:sigterm (signal-handler 'terminate-gracefully)))

(defun terminate-gracefully ()
  (kill-children)
  (sb-ext:quit))

(defun signal-handler (function)
  "Return a signal hander function that will funcall FUNCTION."
  (check-type function (or function symbol))
  (lambda (signal code scp)
    (declare (ignore signal code scp))
    (funcall function)))

;;; Locking
;;;
;;; We serialize the child processes access to the server socket by acquiring an
;;; exclusive file lock around the call to accept.  The advantage of this is
;;; it's really simple and the kernel takes care of everything for us.  The
;;; disadvantage is that the parent process doesn't know what's happening, so it
;;; can't intervene to add or remove child processes.

(defmacro with-lock (fd-spec &body forms)
  (let ((=fd (gensym)))
    `(let ((,=fd ,fd-spec))
       (unwind-protect (progn (flock ,=fd :exclusive)
            ,@forms)
   (flock ,=fd :unlock)))))

(defun serial-accept (socket)
  (with-lock *lock*
    (socket-accept socket)))

(defun setup-lock ()
  (let ((lock-file (format nil "/tmp/~D.lock" (sb-posix:getpid))))
    (unless *lock* (setf *lock* (open lock-file :if-does-not-exist :create)))))

(defun teardown-lock ()
  (when *lock*
    (delete-file *lock*)
    (close *lock*)
    (setf *lock* nil)))

;; I didn't patch SB-POSIX to add flock(2) because it isn't POSIX, it's just a
;; convenient BSD function.

(defmacro defconstant-once (name form &optional doc)
  `(defconstant ,name (if (boundp ',name)
        (symbol-value ',name)
        ,form)
     ,doc))

(defconstant-once +flock-table+
   #((:shared 1) (:exclusive 2) (:nonblocking 4) (:unlock 8)))

(defun flock (fd-spec &rest options)
  "Perform flock(2) on the FD or FILE-STREAM given as FD-SPEC.  OPTIONS are taken from +FLOCK-TABLE+"
  (let* ((options
    (reduce #'logior options
      :key (lambda (option)
       (or (second (find option +flock-table+ :key #'first))
           (error "Unknown flock option: ~S" option)))
      :initial-value 0))
   (fd (etypecase fd-spec
         (sb-alien:int fd-spec)
         (file-stream (sb-sys:fd-stream-fd fd-spec)))))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien
      "flock" (function sb-alien:int sb-alien:int sb-alien:int))
     fd options)))
