(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm")
  ;; Don't warn when I'm hacking the COMM package
  (setf hcl:*packages-for-warn-on-redefinition*
        (remove "COMM" hcl:*packages-for-warn-on-redefinition* :test #'equal)))

(in-package :comm)

(export '(open-udp-stream connect-to-udp-server))

#+win32
(eval-when (:load-toplevel :execute)
  (ensure-sockets))

(defconstant *socket_sock_dgram* 2
  "Connectionless, unreliable datagrams of fixed maximum length.")

(defun open-udp-stream (hostname service &key
			(direction :io)
			(element-type 'base-char)
                        errorp read-timeout write-timeout
			local-address local-port)
  "learn from open-tcp-stream"
  (let ((socket (connect-to-udp-server hostname service
                                       :errorp errorp
                                       :local-address local-address
                                       :local-port local-port)))
    (make-instance 'socket-stream :socket socket
                                  :direction direction
                                  :element-type element-type
                                  :read-timeout read-timeout
                                  :write-timeout write-timeout)))

(defun connect-to-udp-server (host service &key
			      errorp local-address local-port)
  "see connect-to-tcp-server"
  (let ((socket-fd (socket *socket_af_inet*
			   *socket_sock_dgram*
			   *socket_pf_unspec*)))
    (if socket-fd
      (fli:with-dynamic-foreign-objects ((server-addr sockaddr_in)
                                         (client-addr sockaddr_in))
        ;; bind to local address/port if specified.
        (when (and local-address local-port)
          (initialize-sockaddr_in client-addr
				  *socket_af_inet*
				  local-address
				  local-port
				  "udp")
          (bind socket-fd
                (fli:copy-pointer client-addr :type 'sockaddr)
                (fli:pointer-element-size client-addr)))
        ;; connect to remote address/port
        (initialize-sockaddr_in server-addr *socket_af_inet* host service "udp")
        (if (connect socket-fd
                     (fli:copy-pointer server-addr :type 'sockaddr)
                     (fli:pointer-element-size server-addr))
          ;; return
          socket-fd
          (if errorp (error "cannot connect") nil)))
      (if errorp (error "cannot create socket") nil))))
