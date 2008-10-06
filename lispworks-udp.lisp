;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; UDP Support for LispWorks as a COMM package extension

(in-package :comm+)

#+win32
(fli:register-module "ws2_32")

#+win32
(eval-when (:load-toplevel :execute)
  (ensure-sockets))

(defconstant +max-udp-message-size+ 65536)

;;;; Below is something we have to define (others already in COMM package)
;;;; binghe: my design goal is to use what COMM already have, and no C code.

(defconstant *socket_sock_dgram* 2
  "Connectionless, unreliable datagrams of fixed maximum length.")

(defconstant *sockopt_so_rcvtimeo*
  #+(not linux) #x1006
  #+linux 20
  "Socket receive timeout")

(fli:define-c-struct timeval
  (tv-sec :long)
  (tv-usec :long))

;;; ssize_t
;;; recvfrom(int socket, void *restrict buffer, size_t length, int flags,
;;;          struct sockaddr *restrict address, socklen_t *restrict address_len);
(fli:define-foreign-function (%recvfrom "recvfrom" :source)
    ((socket :int)
     (buffer (:pointer (:unsigned :byte)))
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
     (buffer (:pointer (:unsigned :byte)))
     (length :int)
     (flags :int)
     (address (:pointer (:struct sockaddr)))
     (address-len :int))
  :result-type :int)

#-win32
(defmethod set-socket-receive-timeout ((socket-fd integer) seconds)
  "Set socket option: RCVTIMEO, argument seconds can be a float number"
  (declare (type integer socket-fd)
           (type number seconds))
  (multiple-value-bind (sec usec) (truncate seconds)
    (fli:with-dynamic-foreign-objects ((timeout (:struct timeval)))
      (fli:with-foreign-slots (tv-sec tv-usec) timeout
        (setf tv-sec sec
              tv-usec (truncate (* 1000000 usec)))
        (if (zerop (setsockopt socket-fd
                               *sockopt_sol_socket*
                               *sockopt_so_rcvtimeo*
                               (fli:copy-pointer timeout
                                                 :type '(:pointer :void))
                               (fli:size-of '(:struct timeval))))
            seconds)))))

#+win32
(defmethod set-socket-receive-timeout ((socket-fd integer) seconds)
  "Set socket option: RCVTIMEO, argument seconds can be a float number.
   On win32, you must bind the socket before use this function."
  (declare (type integer socket-fd)
           (type number seconds))
  (fli:with-dynamic-foreign-objects ((timeout :int))
    (setf (fli:dereference timeout)
          (truncate (* 1000 seconds)))
    (if (zerop (setsockopt socket-fd
                           *sockopt_sol_socket*
                           *sockopt_so_rcvtimeo*
                           (fli:copy-pointer timeout
                                             :type '(:pointer :char))
                           (fli:size-of :int)))
        seconds)))

#-win32
(defmethod get-socket-receive-timeout ((socket-fd integer))
  "Get socket option: RCVTIMEO, return value is a float number"
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((timeout (:struct timeval))
                                     (len :int))
    (getsockopt socket-fd
                *sockopt_sol_socket*
                *sockopt_so_rcvtimeo*
                (fli:copy-pointer timeout
                                  :type '(:pointer :void))
                len)
    (fli:with-foreign-slots (tv-sec tv-usec) timeout
      (float (+ tv-sec (/ tv-usec 1000000))))))

#+win32
(defmethod get-socket-receive-timeout ((socket-fd integer))
  "Get socket option: RCVTIMEO, return value is a float number"
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((timeout :int)
                                     (len :int))
    (getsockopt socket-fd
                *sockopt_sol_socket*
                *sockopt_so_rcvtimeo*
                (fli:copy-pointer timeout
                                  :type '(:pointer :void))
                len)
    (float (/ (fli:dereference timeout) 1000))))

#+win32
(fli:define-foreign-function (wsa-get-last-error "WSAGetLastError" :source)
    ()
  :result-type :int
  :module "ws2_32")
