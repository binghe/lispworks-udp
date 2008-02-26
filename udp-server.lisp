;;;; UDP Server Support for LispWorks

(in-package :comm)

(defun udp-server-loop (socket-fd &optional (function #'identity))
  (declare (type (function ((simple-array (unsigned-byte 8) (*)))
                           (simple-array (unsigned-byte 8) (*))) function))
  "Main loop for A iterate UDP Server, function type as we declared."
  (mp:ensure-process-cleanup `(close-socket ,socket-fd))
  (let ((message (make-array *max-udp-message-size*
                             :element-type '(unsigned-byte 8)
                             :initial-element 0
                             :allocation :static)))
    (fli:with-dynamic-foreign-objects ((client-addr (:struct sockaddr_in))
                                       (len :int
                                            #+(and lispworks5 (not lispworks5.0))
                                            :initial-element
                                            (fli:size-of '(:struct sockaddr_in))))
      (fli:with-dynamic-lisp-array-pointer (ptr message :type :unsigned-byte)
        (loop (let ((n (%recvfrom socket-fd ptr *max-udp-message-size* 0
                                  (fli:copy-pointer client-addr :type '(:struct sockaddr))
                                  len)))
                (if (plusp n)
                  (let* ((message-in (subseq message 0 n))
                         (message-out (funcall function message-in))
                         (length-out (length message-out)))
                    (replace message message-out)
                    (%sendto socket-fd ptr length-out 0
                             (fli:copy-pointer client-addr :type '(:struct sockaddr))
                             (fli:dereference len))))))))))

(defun start-udp-server (&key (function #'identity) (announce t)
                              (service "lispworks") address
                              (process-name (format nil "~S UDP server" service)))
  "Something like START-UP-SERVER"
  (let ((socket-fd (open-udp-socket :local-address address :local-port service :read-timeout 1)))
    (announce-server-started announce socket-fd nil)
    (mp:process-run-function process-name nil
                             #'udp-server-loop socket-fd function)))

(defmacro with-udp-server ((server &rest args) &body body)
  `(let ((,server (start-udp-server ,@args)))
     (unwind-protect
         (progn ,@body)
       (mp:process-kill ,server))))
