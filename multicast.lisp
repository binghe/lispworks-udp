;;;; -*- Mode: Lisp -*-
;;;; $Id$

;;; UDP Multicat support for LispWorks

(in-package :comm+)

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
