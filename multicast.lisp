;;;; -*- Mode: Lisp -*-
;;;; $Id$

;;; UDP Multicat support for LispWorks

(in-package :comm+)

(defconstant *sockopt_ipproto_ip* 0 "Dummy protocol")

(defconstant *sockopt_ip_multicast_if*
  #-linux  9 #+linux 32
  "specify default interface for outgoing multicasts")

(defconstant *sockopt_ip_multicast_ttl*
  #-linux 10 #+linux 33
  "specify TTL for outgoing multicasts")

(defconstant *sockopt_ip_multicast_loop*
  #-linux 11 #+linux 34
  "enable or disable loopback of outgoing multicasts")

(defconstant *sockopt_ip_add_membership*
  #-linux 12 #+linux 35
  "join a multicast group")

(defconstant *sockopt_ip_drop_membership*
  #-linux 13 #+linux 36
  "leave a multicast group")

;; (fli:size-of '(:struct ip_mreq)) = 8
(fli:define-c-struct ip_mreq
  (imr_multiaddr (:struct in_addr))
  (imr_interface (:struct in_addr)))

#+ignore
(defclass mcast-datagram (inet-datagram)
  ())

(defgeneric mcast-join (socket address &key))

(defmethod mcast-join ((socket inet-datagram) address &key (interface 0) errorp)
  "Join the multicast group (address) on a interface"
  (declare (type (or string integer) address interface))
  (fli:with-dynamic-foreign-objects ((mreq (:struct ip_mreq))
                                     (sock-addr (:struct sockaddr_in)))

    ;; 1. set multiaddr
    (initialize-sockaddr_in sock-addr *socket_af_inet* address 0 "udp")
    (setf (fli:foreign-slot-value
           (fli:foreign-slot-value mreq 'imr_multiaddr
                                   :object-type '(:struct ip_mreq)
                                   :type '(:struct in_addr)
                                   :copy-foreign-object nil)
           's_addr
           :object-type '(:struct in_addr))

          (fli:foreign-slot-value
           (fli:foreign-slot-value sock-addr 'sin_addr
                                   :object-type '(:struct sockaddr_in)
                                   :type '(:struct in_addr)
                                   :copy-foreign-object nil)
           's_addr
           :object-type '(:struct in_addr)))

    ;; 2. set interface
    (initialize-sockaddr_in sock-addr *socket_af_inet* interface 0 "udp")
    (setf (fli:foreign-slot-value
           (fli:foreign-slot-value mreq 'imr_interface
                                   :object-type '(:struct ip_mreq)
                                   :type '(:struct in_addr)
                                   :copy-foreign-object nil)
           's_addr
           :object-type '(:struct in_addr))

          (fli:foreign-slot-value
           (fli:foreign-slot-value sock-addr 'sin_addr
                                   :object-type '(:struct sockaddr_in)
                                   :type '(:struct in_addr)
                                   :copy-foreign-object nil)
           's_addr
           :object-type '(:struct in_addr)))

    ;; 3. call setsockopt()
    (let ((reply (setsockopt (socket-datagram-socket socket)
                             *sockopt_ipproto_ip*
                             *sockopt_ip_add_membership*
                             (fli:copy-pointer mreq :type '(:pointer :char))
                             (fli:size-of '(:struct ip_mreq)))))
      (or (zerop reply)
          (if errorp
              (raise-socket-error "cannot join to multicast group: ~A" address)
            (values nil (get-last-error)))))))

(defmethod mcast-leave ((socket inet-datagram) address)
  )

(defmethod mcast-if ((socket inet-datagram)))

(defmethod mcast-loop ((socket inet-datagram))
  (fli:with-dynamic-foreign-objects ((flag (:unsigned :char))
                                     (len :int))
    (let ((reply (getsockopt (socket-datagram-socket socket)
                             *sockopt_ipproto_ip*
                             *sockopt_ip_multicast_loop*
                             (fli:copy-pointer flag :type '(:pointer :char))
                             len)))
      (values (fli:dereference flag) reply))))

(defmethod mcast-ttl ((socket inet-datagram))
  (fli:with-dynamic-foreign-objects ((ttl (:unsigned :char))
                                     (len :int))
    (let ((reply (getsockopt (socket-datagram-socket socket)
                             *sockopt_ipproto_ip*
                             *sockopt_ip_multicast_ttl*
                             (fli:copy-pointer ttl :type '(:pointer :char))
                             len)))
      (values (fli:dereference ttl) reply))))

(defmethod (setf mcast-ttl) ((ttl integer) (socket inet-datagram))
  (declare (type (integer 0) ttl))
  (fli:with-dynamic-foreign-objects ((%ttl (:unsigned :char)))
    (setf (fli:dereference %ttl) ttl)
    (let ((reply (setsockopt (socket-datagram-socket socket)
                             *sockopt_ipproto_ip*
                             *sockopt_ip_multicast_ttl*
                             (fli:copy-pointer %ttl :type '(:pointer :char))
                             (fli:size-of '(:unsigned :char)))))
      (values ttl reply))))

(defmethod (setf mcast-if) ((interface integer) (socket inet-datagram))
  interface)

(defmethod (setf mcast-if) ((interface string) (socket inet-datagram))
  (setf (mcast-if socket) (string-ip-address interface)))

(defmethod (setf mcast-loop) (flag (socket inet-datagram))
  (setf (mcast-loop socket) (if flag 1 0)))

(defmethod (setf mcast-loop) ((flag integer) (socket inet-datagram))
  (declare (type (integer 0 1) flag))
  (fli:with-dynamic-foreign-objects ((%flag (:unsigned :char)))
    (setf (fli:dereference %flag) flag)
    (let ((reply (setsockopt (socket-datagram-socket socket)
                             *sockopt_ipproto_ip*
                             *sockopt_ip_multicast_loop*
                             (fli:copy-pointer %flag :type '(:pointer :char))
                             (fli:size-of '(:unsigned :char)))))
      (values flag reply))))
