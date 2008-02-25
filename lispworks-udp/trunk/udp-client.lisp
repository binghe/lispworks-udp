;;;; UDP Client Support for LispWorks

;;; TODO: Add RTT support for UDP client, see rtt.lisp

(in-package :comm)

(defun connect-to-udp-server (host service &key errorp local-address local-port)
  "Something like CONNECT-TO-TCP-SERVER"
  (let ((socket-fd (socket *socket_af_inet*
			   *socket_sock_dgram*
			   *socket_pf_unspec*)))
    (if socket-fd
      (fli:with-dynamic-foreign-objects ((server-addr sockaddr_in)
                                         (client-addr sockaddr_in))
        ;; bind to local address/port if specified.
        (when (and local-address local-port)
          (initialize-sockaddr_in client-addr *socket_af_inet* local-address local-port "udp")
          (bind socket-fd
                (fli:copy-pointer client-addr :type 'sockaddr)
                (fli:pointer-element-size client-addr)))
        ;; connect to remote address/port
        (initialize-sockaddr_in server-addr *socket_af_inet* host service "udp")
        (if (connect socket-fd
                     (fli:copy-pointer server-addr :type 'sockaddr)
                     (fli:pointer-element-size server-addr))
          ;; success, return socket fd
          socket-fd
          ;; fail, close socket and return nil
          (progn
            (close-socket socket-fd)
            (if errorp (error "cannot connect") nil))))
      (if errorp (error "cannot create socket") nil))))

(defun open-udp-stream (hostname service &key
                                 (direction :io)
                                 (element-type 'base-char)
                                 errorp
                                 read-timeout
                                 local-address
                                 local-port)
  "Something like OPEN-TCP-STREAM"
  (let ((socket (connect-to-udp-server hostname service
                                       :errorp errorp
                                       :local-address local-address
                                       :local-port local-port)))
    (make-instance 'socket-stream
                   :socket socket
                   :direction direction
                   :element-type element-type
                   :read-timeout read-timeout)))
