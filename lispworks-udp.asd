;;;; -*- Mode: Lisp -*-

(in-package :asdf)

(defsystem lispworks-udp
  :description "UDP support for LispWorks"
  :version "1.0"
  :author "Chun TIAN (binghe) <binghe.lisp@gmail.com>"
  :components ((:file "lispworks-udp")
               (:file "rtt" :depends-on ("lispworks-udp"))))
