;;;; -*- Mode: Lisp -*-

(in-package :cl-user)

(require "comm")

(asdf:defsystem lispworks-udp
  :description "UDP support for LispWorks"
  :version "1.0"
  :author "Chun Tian (binghe) <binghe.lisp@gmail.com>"
  :components ((:file "lispworks-udp")))
