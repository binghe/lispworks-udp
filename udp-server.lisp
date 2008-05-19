;;;; UDP Server Support for LispWorks

(in-package :comm)

(defun default-udp-server-function (data host)
  (declare (ignore host))
  data)

(defun udp-server-loop (socket-fd fn max-buffer-size)
  (declare (type integer socket-fd))
  "Main loop for A iterate UDP Server, function type as we declared."
  (mp:ensure-process-cleanup `(udp-server-loop-cleanup ,socket-fd))
  (let ((message (make-array max-buffer-size
                             :element-type '(unsigned-byte 8)
                             :initial-element 0
                             :allocation :static)))
    (fli:with-dynamic-foreign-objects ((client-addr (:struct sockaddr_in))
                                       (len :int
                                            #+(and lispworks5 (not lispworks5.0))
                                            :initial-element
                                            (fli:size-of '(:struct sockaddr_in))))
      (fli:with-dynamic-lisp-array-pointer (ptr message :type '(:unsigned :byte))
        (loop (let ((n (%recvfrom socket-fd ptr max-buffer-size 0
                                  (fli:copy-pointer client-addr :type '(:struct sockaddr))
                                  len)))
                (if (plusp n)
                  (let* ((client-address
                          (ip-address-string ; translate to string
                           (ntohl (fli:foreign-slot-value
                                   (fli:foreign-slot-value client-addr
                                                           'sin_addr
                                                           :object-type '(:struct sockaddr_in)
                                                           :type '(:struct in_addr)
                                                           :copy-foreign-object nil)
                                   's_addr
                                   :object-type '(:struct in_addr)))))
                         (reply-message (funcall fn (subseq message 0 n) client-address)))
                    (when reply-message ;; or we don't make a reply message
                      (let ((length-out (length reply-message)))
                        (replace message reply-message)
                        (%sendto socket-fd ptr length-out 0
                                 (fli:copy-pointer client-addr :type '(:struct sockaddr))
                                 (fli:dereference len))))))))))))

(defun udp-server-loop-cleanup (process socket-fd)
  (declare (ignore process))
  (close-socket socket-fd))

(defun start-udp-server (&key (function #'default-udp-server-function) (announce nil)
			 address (service "lispworks")
			 (process-name (format nil "~S UDP server" service))
			 (max-buffer-size +max-udp-message-size+))
  "Something like START-UP-SERVER"
  (let ((socket-fd (open-udp-socket :local-address address
                                    :local-port service
                                    :read-timeout 1
                                    :errorp t)))
    (announce-server-started announce socket-fd nil)
    (let ((process (mp:process-run-function process-name nil
                                            #'udp-server-loop
                                            socket-fd function max-buffer-size)))
      (setf (getf (mp:process-plist process) 'socket) socket-fd)
      process)))

(defun stop-udp-server (process &key wait)
  (let ((socket-fd (getf (mp:process-plist process) 'socket)))
    (mp:process-kill process)
    (prog1 (zerop (close-socket socket-fd))
      (when wait
        (mp:process-wait "Wait until UDP server process be killed"
                         #'(lambda () (not (mp:process-alive-p process))))))))

