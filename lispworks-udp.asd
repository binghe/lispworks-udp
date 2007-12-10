;;;; -*- Mode: Lisp -*-

(in-package :asdf)

(require "comm")

(defsystem lispworks-udp
  :description "UDP support for LispWorks"
  :version "1.0"
  :author "Chun TIAN (binghe) <binghe.lisp@gmail.com>"
  :components ((:file "lispworks-udp")))
