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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +max-unix-path-length+ 104))

;; (fli:size-of '(:struct sockaddr_un)) = 106
(fli:define-c-struct sockaddr_un
  (sun_len    (:unsigned :byte))
  (sun_family (:unsigned :byte))
  (sun_path   (:c-array (:unsigned :byte) #.+max-unix-path-length+)))

(defun initialize-sockaddr_un (unaddr family path)
  (declare (type fli::pointer unaddr)
           (type integer family)
           (type string path))
  (let* ((code (ef:encode-lisp-string path :utf-8))
         (len (length code)))
    (fli:fill-foreign-object unaddr
                             :nelems (fli:size-of '(:struct sockaddr_un))
                             :byte 0)
    (setf (fli:foreign-slot-value unaddr 'sun_family) family)
    (fli:replace-foreign-array (fli:foreign-slot-pointer unaddr 'sun_path)
                               code
                               :start1 0 :end1 (min len +max-unix-path-length+))
    (setf (fli:foreign-aref (fli:foreign-slot-pointer unaddr 'sun_path)
                            (min len (1- +max-unix-path-length+)))
          0)))

(defun get-socket-peer-path (socket-fd)
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((sock-addr (:struct sockaddr_un))
                                     (len :int
                                          #-(or lispworks3 lispworks4 lispworks5.0)
                                          :initial-element
                                          (fli:size-of '(:struct sockaddr_un))))
    (getpeername socket-fd (fli:copy-pointer sock-addr :type '(:struct sockaddr)) len)
    (let ((code (make-array (- (fli:dereference len) 2)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
      (fli:replace-foreign-array code (fli:foreign-slot-value sock-addr 'sun_path))
      (ef:decode-external-string code :utf-8))))

(defun connect-to-unix-domain-socket (path &key (errorp nil))
  "Something like CONNECT-TO-TCP-SERVER"
  (let ((socket-fd (socket *socket_af_unix*
			   *socket_sock_stream*
			   *socket_pf_unspec*)))
    (if socket-fd
      (fli:with-dynamic-foreign-objects ((server-addr (:struct sockaddr_un)))
        (initialize-sockaddr_un server-addr *socket_af_unix* path)
        (if (connect socket-fd
                     (fli:copy-pointer server-addr :type '(:struct sockaddr))
                     (fli:pointer-element-size server-addr))
          ;; success
          socket-fd
          ;; fail
          (progn
            (close-socket socket-fd)
            (when errorp
              (error 'socket-error
                     :format-string "cannot connect")))))
      (when errorp
        (error 'socket-error
               :format-string "cannot create socket")))))

(defun open-unix-stream (path &key (direction :io)
                              (element-type 'base-char)
                              errorp read-timeout)
  "Open a UNIX domain socket stream"
  (let ((socket-fd (connect-to-unix-domain-socket path :errorp errorp)))
    (if socket-fd
      (make-instance 'comm:socket-stream
                     :socket socket-fd
                     :element-type element-type
                     :direction direction
                     :read-timeout read-timeout))))

(defmacro with-unix-stream ((stream &rest options) &body body)
  `(let ((,stream (open-unix-stream ,@options)))
     (unwind-protect
         (progn ,@body)
       (close ,stream))))
