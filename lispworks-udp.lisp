(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm")
  (require "foreign-parser")
  ;; Don't warn when I hacking COMM package
  (setf hcl:*packages-for-warn-on-redefinition*
        (remove "COMM" hcl:*packages-for-warn-on-redefinition* :test #'equal)))

(in-package :comm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(socket-datagram       ; socket-stream
            open-udp-datagram     ; open-tcp-socket
            connect-to-udp-server ; connect-to-tcp-server
            )))

(defun make-udp-dff ()
  (foreign-parser:process-foreign-file #+unix "lispworks-udp-unix.h"
                                       #+win32 "lispworks-udp-win32.h"
                                       :case-sensitive nil
                                       :package :comm))

#|
(:STRUCT COMM::SOCKADDR_IN)
COMM::INITIALIZE-SOCKADDR_IN
(INITIALIZE-SOCKADDR_IN INADDR FAMILY HOST SERVICE PROTOCOL)
COMM::*SOCKET_SOCK_STREAM*
COMM::*SOCKET_PF_UNSPEC*
COMM::SOCKET
COMM::BIND
COMM::*SOCKOPT_TCP_NODELAY*
COMM::*SOCKOPT_IPPROTO_TCP*
COMM::SETSOCKOPT
SYSTEM::GET-OS-ERROR-STRING
COMM::SET-SOCKET-NO-BLOCKING
(:STRUCT COMM::SOCKADDR)
COMM::CONNECT
COMM::CLOSE-SOCKET
COMM::WAIT-FOR-CONNECT-TO-COMPLETE
COMM::GETHOSTBYNAME
COMM::INET_ADDR
COMM::H_ADDRTYPE
COMM::SIN_FAMILY
(:STRUCT COMM:IN_ADDR)
COMM::HTONL
COMM::GET-PORT-FOR-SERVICE
comm::*socket_af_inet*
comm::*socket_sock_stream* (need comm::*socket_sock_dgram*)
COMM::FCNTL
|#

(defconstant *socket_pf_inet* 2 "IP protocol family")
(defconstant *socket_sock_dgram* 2
  "Connectionless, unreliable datagrams of fixed maximum length.")

(defclass socket-datagram (socket-stream)
  ()
  (:documentation "UDP Socket"))

(defun open-udp-datagram (hostname
                          service
                          &key
                          (direction :io)
                          (element-type 'base-char)
                          errorp
                          read-timeout
                          write-timeout
                          timeout
                          local-address
                          local-port)
  "learn from open-tcp-stream"
  (let ((socket (connect-to-udp-server hostname service
                                       :errorp errorp
                                       :timeout timeout
                                       :local-address local-address
                                       :local-port local-port)))
    (unwind-protect
        (make-instance 'socket-datagram
                       :init t
                       :socket socket
                       :direction direction
                       :element-type element-type
                       :read-timeout read-timeout
                       :write-timeout write-timeout)
      (close-socket socket))))

(defun connect-to-udp-server (host service &key errorp timeout local-address local-port)
  "see connect-to-tcp-server"
  
  )
