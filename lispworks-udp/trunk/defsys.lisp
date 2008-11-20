;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; System Definition for LispWorks UDP

(in-package :cl-user)

;;; Load COMM package
(require "comm")

(defsystem lispworks-udp
  (:optimize ((safety 3) (debug 3)))
  :members (package
            rtt
            lispworks-udp
            class
            wait-for-input
	    condition
            udp-client
            udp-server
            rtt-client
            unix
            unix-server)
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))))
