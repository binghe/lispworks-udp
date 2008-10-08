;;;; -*- Mode: Lisp -*-
;;;; $Id$

(in-package :cl-user)

(defpackage comm+
  (:use :common-lisp :lispworks :comm)

  (:import-from :comm
   #:*socket_af_inet*
   #:*socket_af_unix*
   #:*socket_pf_unspec*
   #:*socket_sock_stream*
   #:*sockopt_sol_socket*
   #:%send
   #:announce-server-started
   #:bind
   #:close-socket
   #:connect
   #:getpeername
   #:getsockname
   #:getsockopt
   #:in_addr
   #:initialize-sockaddr_in
   #:ntohl
   #:ntohs
   #:s_addr
   #:setsockopt
   #:sin_addr
   #:sin_port
   #:sockaddr
   #:sockaddr_in
   #:socket
   #+win32 #:ensure-sockets)

  (:export
   #:*client-address*
   #:*client-port*
   #:*rtt-maxnrexmt*
   #:*rtt-rxtmax*
   #:*rtt-rxtmin*
   #:close-udp-socket
   #:connect-to-udp-server
   #:connect-to-unix-domain-socket
   #:get-socket-receive-timeout
   #:send-message
   #:socket-datagram
   #:socket-datagram-socket
   #:open-udp-socket
   #:open-udp-stream
   #:open-unix-stream
   #:receive-message
   #:set-socket-receive-timeout
   #:start-udp-server
   #:stop-udp-server
   #:sync-message
   #:with-connected-udp-socket
   #:with-udp-socket
   #:with-udp-stream))

(in-package :comm+)

;;; Export all external symbols of COMM
(eval-when (:load-toplevel :execute)
  (do-external-symbols (symbol (find-package :comm))
    (export symbol)))
