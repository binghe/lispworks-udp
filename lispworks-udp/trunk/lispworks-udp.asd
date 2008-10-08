;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; System Definition for LispWorks UDP

(in-package :asdf)

;;; Load COMM package
(require "comm")

(defsystem lispworks-udp
  :description "UDP support for LispWorks"
  :license "MIT"
  :version "4.0"
  :author "Chun Tian (binghe) <binghe.lisp@gmail.com>"
  :serial t
  :components ((:file "package")
               (:file "rtt")
               (:file "lispworks-udp")
               (:file "class")
               (:file "wait-for-input")
	       (:file "condition")
               (:file "udp-client")
               (:file "udp-server")
               (:file "rtt-client")
               (:file "multicast")
               #+(not win32) (:file "unix")
               #+(not win32) (:file "unix-server")))
