;;;; UDP Server Support for LispWorks

(in-package :comm)

;;; Exports into COMM Package
(export '(start-up-udp-server))

(defun udp-server-loop (socket function)
  "Main loop for A iterate UDP Server"
  (loop
   ;;; haven't implemented
   ))

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
          socket-fd
          (progn
            (close-socket socket-fd)
            (error "cannot bind"))))
      (error "cannot create socket"))))

(defun start-up-udp-server (&key function
                                 (announce t)
                                 (service "lispworks")
                                 address
                                 (process-name (format nil "~S UDP server" service)))
  "Something like START-UP-SERVER"
  (let ((socket (create-udp-socket-for-service service :address address)))
    (announce-server-started announce socket nil)
    (mp:process-run-function process-name
                             '(:priority 0)
                             #'udp-server-loop socket function)))
