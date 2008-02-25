;;;; UDP Support for LispWorks as a COMM package extension

(in-package :comm)

#+win32
(eval-when (:load-toplevel :execute)
  (ensure-sockets))

;;; Below is something we have to define (others can be found in COMM package)

;;; ssize_t
;;; recvfrom(int socket, void *restrict buffer, size_t length, int flags,
;;;          struct sockaddr *restrict address, socklen_t *restrict address_len);
(fli:define-foreign-function (%recvfrom "recvfrom" :source)
    ((socket :int)
     (buffer (:pointer :void))
     (length :size-of-type)
     (flags :int)
     (address (:pointer (:struct 'sockaddr)))
     (address-len (:pointer :size-of-type)))
  :result-type :int)

(defconstant *socket_sock_dgram* 2
  "Connectionless, unreliable datagrams of fixed maximum length.")

