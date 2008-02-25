;;;; -*- Mode: Lisp -*-

(in-package :asdf)

(require "comm")
;; Don't warn when I'm hacking the COMM package
(setf hcl:*packages-for-warn-on-redefinition*
      (remove "COMM" hcl:*packages-for-warn-on-redefinition* :test #'equal))

(defsystem lispworks-udp
  :description "UDP support for LispWorks"
  :version "1.0"
  :author "Chun TIAN (binghe) <binghe.lisp@gmail.com>"
  :serial t
  :components ((:file "lispworks-udp")
               (:file "udp-client")
               (:file "udp-server")
               (:file "rtt")
               (:file "unix")))
