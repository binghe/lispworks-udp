(in-package :cl-user)

;;; UDP Echo Test: use macros
(defun udp-echo-test-3 (&optional (port 10000))
  (let* ((fn #'(lambda (data host)
                 (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                      (format nil "receive from ~A: ~A" host (map 'string #'code-char data)))))
         (server-process (comm:start-udp-server :function fn :service port)))
    (unwind-protect
        (comm:with-udp-stream (stream "localhost" port :read-timeout 1)
          (let ((data "Hello, world!"))
            (format stream "~A" data)
            (terpri stream) ;; = "~%" or #\Newline
            (force-output stream)
            (format t "STREAM: Send message: ~A~%" data)
            (let ((echo (read-line stream nil nil)))
              (format t "STREAM: Recv message: ~A~%" echo))))
      (mp:process-kill server-process))))

(defun udp-echo-test-4 (&optional (port 10000))
  (let* ((echo-fn #'(lambda (data host)
                      (declare (ignore host))
                      data))
         (server-process (comm:start-udp-server :function echo-fn :service port)))
    (unwind-protect
        (comm:with-udp-socket (socket :read-timeout 1)
          (let ((data #(1 2 3 4 5 6 7 8 9 10)))
            (comm:send-message socket "localhost" port data)
            (format t "SOCKET: Send message: ~A~%" data)
            (let ((echo (comm:receive-message socket)))
              (format t "SOCKET: Recv message: ~A~%" echo))))
      (mp:process-kill server-process))))

(defun loop-test ()
  (labels ((echo-fn (data host)
             (declare (ignore host))
             data))
    (loop for i from 1 to 10
          do (let ((server (comm:start-udp-server :function #'echo-fn :service 3500)))
               (comm:with-udp-socket (socket :read-timeout 1)
                 (let ((data #(1 2 3 4 5 6 7 8 9 10)))
                   (comm:send-message socket "localhost" 3500 data)
                   (format t "SOCKET: Send message: ~A~%" data)
                   (let ((echo (comm:receive-message socket)))
                     (format t "SOCKET: Recv message: ~A~%" echo))))
               (princ (comm:stop-udp-server server :wait t))))))
