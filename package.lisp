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
   #:socket)

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
   #:open-unix-domain-stream
   #:receive-message
   #:set-socket-receive-timeout
   #:start-udp-server
   #:stop-udp-server
   #:sync-message
   #:with-connected-udp-socket
   #:with-udp-socket
   #:with-udp-stream

   ;; Also export all external symbol of COMM
   #:attach-ssl
   #:destroy-ssl
   #:destroy-ssl-ctx
   #:detach-ssl
   #:do-rand-seed
   #:ensure-ssl
   #:get-host-entry
   #:get-socket-address
   #:get-socket-peer-address
   #:get-verification-mode
   #:ip-address-string
   #:make-ssl-ctx
   #:open-tcp-stream
   #:openssl-version
   #:pem-read
   #:read-dhparams
   #:set-verification-mode
   #:set-ssl-ctx-dh
   #:set-ssl-ctx-options
   #:set-ssl-ctx-password-callback
   #:set-ssl-library-path
   #:socket-error
   #:socket-stream
   #:socket-stream-address
   #:socket-stream-ctx
   #:socket-stream-peer-address
   #:socket-stream-ssl
   #:ssl-cipher-pointer
   #:ssl-cipher-pointer-stack
   #:ssl-closed
   #:ssl-condition
   #:ssl-ctx-pointer
   #:ssl-error
   #:ssl-failure
   #:ssl-new
   #:ssl-pointer
   #:ssl-x509-lookup
   #:start-up-server
   #:start-up-server-and-mp
   #:string-ip-address
   #:with-noticed-socket-stream))
