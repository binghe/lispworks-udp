(in-package :cl-user)

;;; UDP Echo Test: Client as a STREAM
(defun udp-echo-test (&optional (port 10000) (function #'identity))
  (let ((server-process (comm:start-udp-server :function function :service port))
        (client-stream (comm:open-udp-stream "localhost" port :read-timeout 1))
        (data "Hello, World!"))
    (unwind-protect
        (dotimes (i 5)
          (format client-stream "~A" data)
          (terpri client-stream) ;; = "~%" or #\Newline
          (force-output client-stream)
          (format t "~D: Send message: ~A~%" i data)
          (let ((echo (read-line client-stream nil nil)))
            (format t "~D: Recv message: ~A~%" i echo)))
      (progn
        (close client-stream)
        (mp:process-kill server-process)))))

;;; UDP Reverse-Echo Test: Client as a MESSAGE send/receive
(defun udp-echo-test-2 (&optional (port 10000) (function #'reverse))
  (let ((server-process (comm:start-udp-server :function function :service port))
        (client-socket (comm:open-udp-socket :read-timeout 1))
        (data #(1 2 3 4 5 6 7 8 9 10)))
    (unwind-protect
        (dotimes (i 5)
          (comm:send-message client-socket "localhost" port data)
          (format t "~D: Send message: ~A~%" i data)
          (let ((echo (comm:receive-message client-socket)))
            (format t "~D: Recv message: ~A~%" i echo)))
      (progn
        (comm::close-socket client-socket)
        (mp:process-kill server-process)))))

;;; UDP Echo Test: use macros
(defun udp-echo-test-3 (&optional (port 10000) (function #'identity))
  (comm:with-udp-server (server :function function :service port)
    ;;; stream test
    (comm:with-udp-stream (stream "localhost" port :read-timeout 1)
      (let ((data "Hello, world!"))
        (format stream "~A" data)
        (terpri stream) ;; = "~%" or #\Newline
        (force-output stream)
        (format t "STREAM: Send message: ~A~%" data)
        (let ((echo (read-line stream nil nil)))
          (format t "STREAM: Recv message: ~A~%" echo))))
    ;; message test
    (comm:with-udp-socket (socket :read-timeout 1)     
      (let ((data #(1 2 3 4 5 6 7 8 9 10)))
        (comm:send-message socket "localhost" port data)
        (format t "SOCKET: Send message: ~A~%" data)
        (let ((echo (comm:receive-message socket)))
          (format t "SOCKET: Recv message: ~A~%" echo))))))
