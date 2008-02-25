;;;; UDP Server Support for LispWorks

(in-package :comm)

(defparameter *max-udp-message-size* 65536)

(defun set-socket-receive-timeout (socket-fd sec usec)
  "Set socket option: REVTIMEO, not implemented"
  (declare (ignore socket-fd sec usec))
  t) ; (setsockopt *sockopt_sol_socket* *sockopt_so_rcvtimeo* ...)

(defun udp-server-loop (socket-fd &optional (function #'identity))
  (declare (type (function ((simple-array (unsigned-byte 8) (*)))
                           (simple-array (unsigned-byte 8) (*))) function))
  "Main loop for A iterate UDP Server, function type as we declared."
  (let ((message (make-array *max-udp-message-size*
                             :element-type '(unsigned-byte 8)
                             :initial-element 0
                             :allocation :static)))
    (fli:with-dynamic-foreign-objects ((client-addr sockaddr_in)
                                       (len :int
                                            #+(and lispworks5 (not lispworks5.0))
                                            :initial-element
                                            (fli:size-of 'sockaddr_in)))
      (fli:with-dynamic-lisp-array-pointer (ptr message :type :unsigned-byte)
        (loop (let ((n (%recvfrom socket-fd ptr *max-udp-message-size* 0
                                  (fli:copy-pointer client-addr :type 'sockaddr) len)))
                (if (plusp n)
                  (let* ((message-in (subseq message 0 n))
                         (message-out (funcall function message-in))
                         (length-out (length message-out)))
                    (replace message message-out)
                    (%sendto socket-fd ptr length-out 0
                             (fli:copy-pointer client-addr :type 'sockaddr)
                             (fli:dereference len))))))))))

(defun create-udp-socket-for-service (service &key address)
  "Something like CREATE-TCP-SOCKET-FOR-SERVICE"
  (let ((socket-fd (socket *socket_af_inet*
			   *socket_sock_dgram*
			   *socket_pf_unspec*)))
    (if socket-fd
      (fli:with-dynamic-foreign-objects ((server-addr sockaddr_in))
        (initialize-sockaddr_in server-addr *socket_af_inet* address service "udp")
        (if (bind socket-fd
                  (fli:copy-pointer server-addr :type 'sockaddr)
                  (fli:pointer-element-size server-addr))
          (progn
            (set-socket-receive-timeout socket-fd 1 0)
            socket-fd)
          (progn
            (close-socket socket-fd)
            (error "cannot bind"))))
      (error "cannot create socket"))))

(defun start-udp-server (&key (function #'identity)
                              (announce nil)
                              (service "lispworks")
                              address
                              (process-name (format nil "~S UDP server" service)))
  "Something like START-UP-SERVER"
  (let ((socket-fd (create-udp-socket-for-service service :address address)))
    (announce-server-started announce socket-fd nil)
    (list (mp:process-run-function process-name
                                   '(:priority 0)
                                   #'udp-server-loop socket-fd function)
          socket-fd)))

(defun stop-udp-server (server)
  (destructuring-bind (process socket-fd) server
    (close-socket socket-fd)
    (mp:process-kill process)))
