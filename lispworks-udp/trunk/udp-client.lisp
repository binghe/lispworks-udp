;;;; UDP Client Support for LispWorks

;;; TODO: Add RTT support for UDP client, see rtt.lisp

(in-package :comm)

(defun open-udp-socket (&key errorp local-address local-port read-timeout)
  "Open a unconnected UDP socket"
  (let ((socket-fd (socket *socket_af_inet* *socket_sock_dgram* *socket_pf_unspec*)))
    (if socket-fd
      (progn
        (when read-timeout (set-socket-receive-timeout socket-fd read-timeout))
        (if local-port
          (progn ;; bind to local address/port if specified.
            (fli:with-dynamic-foreign-objects ((client-addr (:struct sockaddr_in)))
              (initialize-sockaddr_in client-addr *socket_af_inet* local-address local-port "udp")
              (if (bind socket-fd
                        (fli:copy-pointer client-addr :type '(:struct sockaddr))
                        (fli:pointer-element-size client-addr))
                ;; success, return socket fd
                socket-fd
                (progn ;; fail, close socket and return nil
                  (close-socket socket-fd)
                  (if errorp (error "cannot bind local address/port") nil)))))
          socket-fd))
      (if errorp (error "cannot create socket") nil))))

(defmacro with-udp-socket ((socket &rest args) &body body)
  `(let ((,socket (open-udp-socket ,@args)))
     (unwind-protect
         (progn ,@body)
       (close-socket ,socket))))

(defun send-message (socket buffer &optional (length (length buffer)) host service)
  "Send message to a socket, using sendto()/send()"
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum length))
  (let ((message (make-array *max-udp-message-size*
                             :element-type '(unsigned-byte 8)
                             :initial-element 0
                             :allocation :static)))
    (fli:with-dynamic-foreign-objects ((client-addr (:struct sockaddr_in))
                                       (len :int
                                            #+(and lispworks5 (not lispworks5.0))
                                            :initial-element
                                            (fli:size-of '(:struct sockaddr_in))))
      (fli:with-dynamic-lisp-array-pointer (ptr message :type '(:unsigned :byte))
        (replace message buffer :end2 length)
        (if (and host service)
          (progn
            (initialize-sockaddr_in client-addr *socket_af_inet* host service "udp")
            (%sendto socket ptr (min length *max-udp-message-size*) 0
                     (fli:copy-pointer client-addr :type '(:struct sockaddr))
                     (fli:dereference len)))
          (%send socket ptr (min length *max-udp-message-size*) 0))))))

(defun receive-message (socket &optional buffer (length (length buffer)) &key read-timeout)
  "Receive message from socket, this function will return 4 values:
   1. receive buffer
   2. number of receive bytes
   3. remote address
   4. remote port"
  (let ((message (make-array *max-udp-message-size*
                             :element-type '(unsigned-byte 8)
                             :initial-element 0
                             :allocation :static)))
    (fli:with-dynamic-foreign-objects ((client-addr (:struct sockaddr_in))
                                       (len :int
                                            #+(and lispworks5 (not lispworks5.0))
                                            :initial-element
                                            (fli:size-of '(:struct sockaddr_in))))
      (fli:with-dynamic-lisp-array-pointer (ptr message :type '(:unsigned :byte))
        (when read-timeout (set-socket-receive-timeout socket read-timeout))
        (let ((n (%recvfrom socket ptr *max-udp-message-size* 0
                            (fli:copy-pointer client-addr :type '(:struct sockaddr))
                            len)))
          (if (plusp n)
            (values (if buffer
                      (replace buffer message :end1 length :end2 n)
                      (subseq message 0 n))
                    n
                    (ip-address-string ; translate to string
                     (ntohl (fli:foreign-slot-value
                             (fli:foreign-slot-value client-addr
                                                     'sin_addr
                                                     :object-type '(:struct sockaddr_in)
                                                     :type '(:struct in_addr)
                                                     :copy-foreign-object nil)
                             's_addr
                             :object-type '(:struct in_addr))))
                    (ntohs (fli:foreign-slot-value client-addr
                                                   'sin_port
                                                   :object-type '(:struct sockaddr_in)
                                                   :type '(:unsigned :short)
                                                   :copy-foreign-object nil)))
            (values nil 0 "" 0)))))))

(defun connect-to-udp-server (hostname service &key errorp local-address local-port read-timeout)
  "Something like CONNECT-TO-TCP-SERVER"
  (let ((socket (open-udp-socket :errorp errorp
                                 :local-address local-address
                                 :local-port local-port
                                 :read-timeout read-timeout)))
    (if socket
      (fli:with-dynamic-foreign-objects ((server-addr (:struct sockaddr_in)))
        ;; connect to remote address/port
        (initialize-sockaddr_in server-addr *socket_af_inet* hostname service "udp")
        (if (connect socket
                     (fli:copy-pointer server-addr :type '(:struct sockaddr))
                     (fli:pointer-element-size server-addr))
          ;; success, return socket fd
          socket
          ;; fail, close socket and return nil
          (progn
            (close-socket socket)
            (if errorp (error "cannot connect") nil))))
      (if errorp (error "cannot create socket") nil))))

(defmacro with-connected-udp-socket ((socket &rest args) &body body)
  `(let ((,socket (connect-to-udp-server ,@args)))
     (unwind-protect
         (progn ,@body)
       (close-socket ,socket))))

(defun open-udp-stream (hostname service &key (direction :io) (element-type 'base-char)
                                 errorp read-timeout local-address local-port)
  "Something like OPEN-TCP-STREAM"
  (let ((socket (connect-to-udp-server hostname service
                                       :errorp errorp
                                       :local-address local-address
                                       :local-port local-port)))
    (make-instance 'socket-stream
                   :socket socket
                   :direction direction
                   :element-type element-type
                   :read-timeout read-timeout)))

(defmacro with-udp-stream ((stream &rest args) &body body)
  `(let ((,stream (open-udp-stream ,@args)))
     (unwind-protect
         (progn ,@body)
       (close ,stream))))
