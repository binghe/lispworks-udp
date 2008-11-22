;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; ICMP support for LispWorks

(in-package :comm+)

(defconstant *socket_sock_raw* 3)
(defconstant *sockopt_ipproto_icmp* 1 "control message protocol")
(defconstant *sockopt_ipproto_raw* 255 "raw IP packet")

(defun open-icmp-socket ()
  (let ((socket-fd (socket *socket_af_inet* *socket_sock_raw* *sockopt_ipproto_icmp*)))
    socket-fd))
