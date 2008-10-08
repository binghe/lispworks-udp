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
    #:*sockopt_so_reuseaddr*
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
    #+win32 #:ensure-sockets
    #+win32 #:wsa-get-last-error
    #+win32 #:wsa-event-select
    #+win32 #:wsa-cleanup)
  (:export
    #:*client-address*
    #:*client-port*
    #:*rtt-maxnrexmt*
    #:*rtt-rxtmax*
    #:*rtt-rxtmin*
    #:close-datagram
    #:connect-to-udp-server
    #:connect-to-unix-path
    #:get-socket-pathname
    #:get-socket-peer-pathname
    #:inet-datagram ; class
    #:mcast-datagran ; class
    #:open-udp-socket
    #:open-udp-stream
    #:open-unix-socket
    #:open-unix-stream
    #:receive-message
    #:send-message
    #:socket-datagram ; class
    #:socket-datagram-socket
    #:socket-receive-timeout
    #:start-udp-server
    #:stop-udp-server
    #:sync-message
    #:unix-datagram ; class
    #:with-connected-udp-socket
    #:with-connected-unix-socket
    #:with-udp-socket
    #:with-udp-stream
    #:with-unix-socket))

(in-package :comm+)

;;; Export all external symbols of COMM
(eval-when (:load-toplevel :execute)
  (do-external-symbols (symbol (find-package :comm))
    (export symbol)))
