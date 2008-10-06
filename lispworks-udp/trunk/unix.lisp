;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; UNIX Domain Socket support for LispWorks

;;; binghe: I took source code from http://www.bew.org.uk/Lisp/
;;; and modify it into a pure lisp package without C code which can be used
;;; without a C compiler (for example, most Windows and some Macintosh)

;;; Original license of UNIX Domain Socket support for LispWorks:

;;; Copyright 2001, Barry Wilkes <bew@bcs.org.uk>
;;; uk.org.bew.comm-ext, an extension to the network interface for LispWorks/Linux
;;; 
;;; uk.org.bew.comm-ext is licensed under the terms of the Lisp Lesser GNU
;;; Public License (http://opensource.franz.com/preamble.html), known as
;;; the LLGPL.  The LLGPL consists of a preamble (see above URL) and the
;;; LGPL.  Where these conflict, the preamble takes precedence. 
;;; uk.org.bew.comm-ext is referenced in the preamble as the "LIBRARY."

(in-package :comm+)

(defun connect-to-unix-domain-socket (source &key (errorp nil))
  "Something like CONNECT-TO-TCP-SERVER"
  (let ((socket-fd (socket *socket_af_unix*
			   *socket_sock_stream*
			   *socket_pf_unspec*)))
    (if socket-fd
      nil
      (if errorp (error "Cannot create UNIX domain socket") nil))))

(defun open-unix-domain-stream (source &key
                                       (direction :io)
                                       (element-type 'base-char)
                                       (errorp nil)
                                       timeout)
  (let ((socket (connect-to-unix-domain-socket source :errorp errorp)))
    (if (= socket -1)
      (if errorp
        (error "Failed to create unix domain socket ~S" source)
        nil)
      (make-instance 'comm:socket-stream
                     :socket socket
                     :element-type element-type
                     :direction direction))))
