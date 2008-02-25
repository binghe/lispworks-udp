;;;; UDP Support for LispWorks as a COMM package extension

(in-package :comm)

#+win32
(eval-when (:load-toplevel :execute)
  (ensure-sockets))

;;; Exports into COMM Package
(export '(;; UDP Client
          open-udp-stream
          connect-to-udp-server
          ;; UDP Server
          start-udp-server
          stop-udp-server
          ;; UNIX Domain Socket
          open-unix-domain-stream
          connect-to-unix-domain-socket))

;;;; Below is something we have to define (others already in COMM package)
;;;; binghe: my design goal is to use what COMM already have, and no C code.

(defconstant *socket_sock_dgram* 2
  "Connectionless, unreliable datagrams of fixed maximum length.")
(defconstant *sockopt_so_rcvtimeo* #x1006 "receive timeout")

;;; ssize_t
;;; recvfrom(int socket, void *restrict buffer, size_t length, int flags,
;;;          struct sockaddr *restrict address, socklen_t *restrict address_len);
(fli:define-foreign-function (%recvfrom "recvfrom" :source)
    ((socket :int)
     (buffer (:pointer :unsigned-byte))
     (length :int)
     (flags :int)
     (address (:pointer (:struct sockaddr)))
     (address-len (:pointer :int)))
  :result-type :int)

;;; ssize_t
;;; sendto(int socket, const void *buffer, size_t length, int flags,
;;;        const struct sockaddr *dest_addr, socklen_t dest_len);
(fli:define-foreign-function (%sendto "sendto" :source)
    ((socket :int)
     (buffer (:pointer :unsigned-byte))
     (length :int)
     (flags :int)
     (address (:pointer (:struct sockaddr)))
     (address-len :int))
  :result-type :int)

