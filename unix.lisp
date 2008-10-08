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
  (declare (type (satisfies fli:pointerp) unaddr)
           (type integer family)
           (type string path))
    (fli:fill-foreign-object unaddr :byte 0)
    (let* ((code (ef:encode-lisp-string path :utf-8))
           (len (length code)))
      (fli:with-foreign-slots (sun_len sun_family) unaddr
        (setf sun_len (+ len 2)
              sun_family family))
      (fli:replace-foreign-array (fli:foreign-slot-pointer unaddr 'sun_path)
                                 code
                                 :start1 0 :end1 (min len +max-unix-path-length+))
      (setf (fli:foreign-aref (fli:foreign-slot-pointer unaddr 'sun_path)
                              (min len (1- +max-unix-path-length+)))
            0)
      code))

(defun get-socket-peer-pathname (socket-fd)
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((sock-addr (:struct sockaddr_un))
                                     (len :int
                                          #-(or lispworks3 lispworks4 lispworks5.0)
                                          :initial-element
                                          (fli:size-of '(:struct sockaddr_un))))
    (fli:fill-foreign-object sock-addr :byte 0)
    (let ((return-code
           (getpeername socket-fd (fli:copy-pointer sock-addr :type '(:struct sockaddr)) len)))
      (when (zerop return-code)
        (fli:convert-from-foreign-string (fli:foreign-slot-pointer sock-addr 'sun_path)
                                         :external-format :utf-8
                                         :null-terminated-p t
                                         :allow-null t)))))

(defun get-socket-pathname (socket-fd)
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((sock-addr (:struct sockaddr_un))
                                     (len :int
                                          #-(or lispworks3 lispworks4 lispworks5.0)
                                          :initial-element
                                          (fli:size-of '(:struct sockaddr_un))))
    (fli:fill-foreign-object sock-addr :byte 0)
    (let ((return-code
           (getsockname socket-fd (fli:copy-pointer sock-addr :type '(:struct sockaddr)) len)))
      (when (zerop return-code)
        (fli:convert-from-foreign-string (fli:foreign-slot-pointer sock-addr 'sun_path)
                                         :external-format :utf-8
                                         :null-terminated-p t
                                         :allow-null t)))))

(defun open-unix-socket (&key (protocol :datagram)
                              errorp local-pathname read-timeout)
  (let ((socket-fd (socket *socket_af_unix*
			   (ecase protocol
                             (:stream *socket_sock_stream*)
                             (:datagram *socket_sock_dgram*))
			   *socket_pf_unspec*)))
    (if socket-fd
      (progn
        (when read-timeout (set-socket-receive-timeout socket-fd read-timeout))
        (if local-pathname
          (progn
            (fli:with-dynamic-foreign-objects ((client-addr (:struct sockaddr_un)))
              (initialize-sockaddr_in client-addr *socket_af_unix*
                                      (namestring (truename local-pathname)))
              (if (bind socket-fd
                        (fli:copy-pointer client-addr :type '(:struct sockaddr))
                        (fli:pointer-element-size client-addr))
                ;; success, return socket fd
                (ecase protocol
                  (:stream socket-fd)
                  (:datagram (make-datagram socket-fd)))
                (progn ;; fail, close socket and return nil
                  (close-socket socket-fd)
                  (when errorp
                    (error 'socket-error
                           :format-string "cannot bind local pathname"))))))
          (ecase protocol
            (:stream socket-fd)
            (:datagram (make-datagram socket-fd)))))
      (when errorp (error 'socket-error "cannot create socket")))))

(defun connect-to-unix-pathname (pathname &key (protocol :datagram)
                                               errorp read-timeout)
  "Something like CONNECT-TO-TCP-SERVER"
  (declare (type (or pathname string) pathname))
  (let ((socket (open-unix-socket :protocol protocol
                                  :errorp errorp
                                  :read-timeout read-timeout)))
    (if socket
      (let ((socket-fd (ecase protocol
                         (:stream socket)
                         (:datagram (socket-datagram-socket socket)))))
        (fli:with-dynamic-foreign-objects ((server-addr (:struct sockaddr_un)))
          (initialize-sockaddr_un server-addr *socket_af_unix*
                                  (namestring (truename pathname)))
          (if (connect socket-fd
                       (fli:copy-pointer server-addr :type '(:struct sockaddr))
                       (fli:pointer-element-size server-addr))
            ;; success
            (ecase protocol
              (:stream socket-fd)
              (:datagram socket))
            ;; fail
            (progn
              (ecase protocol
                (:stream (close-socket socket-fd))
                (:datagram (close-datagram socket)))
              (when errorp
                (error 'socket-error
                       :format-string "cannot connect")))))
        (when errorp
          (error 'socket-error
                 :format-string "cannot create socket"))))))

(defun open-unix-stream (pathname &key (direction :io)
                                  (element-type 'base-char)
                                  errorp read-timeout)
  "Open a UNIX domain socket stream"
  (declare (type (or pathname string) pathname))
  (let ((socket-fd (connect-to-unix-pathname pathname
                                             :protocol :stream
                                             :errorp errorp)))
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
