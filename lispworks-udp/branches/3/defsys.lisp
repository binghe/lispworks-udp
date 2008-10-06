;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; System Definition for LispWorks UDP

(in-package :cl-user)

;;; Load COMM package
(require "comm")

;;; Don't warn when I'm hacking the COMM package
(setf hcl:*packages-for-warn-on-redefinition*
      (remove "COMM" hcl:*packages-for-warn-on-redefinition* :test #'equal))

(defsystem lispworks-udp
  (:package :comm
   :optimize ((safety 3) (debug 3)))
  :members (lispworks-udp
            rtt
	    condition
            udp-client
            udp-server
            rtt-client
            unix)
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))))
