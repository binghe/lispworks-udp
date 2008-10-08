;;;; -*- Mode: Lisp -*-
;;;; $Id$

(in-package :comm+)

(defclass socket-datagram ()
  ((open-p :type boolean
           :accessor socket-open-p
           :initform t)
   (socket :type integer
           :reader socket-datagram-socket
           :initarg :socket))
  (:documentation "datagram socket class"))

(defclass inet-datagram (socket-datagram rtt-info-mixin)
  ())

(defclass unix-datagram (socket-datagram)
  ((local-pathname :type (or null string pathname)
                   :accessor unix-datagram-pathname
                   :initarg :pathname)))

(defun make-inet-datagram (socket-fd)
  (make-instance 'inet-datagram :socket socket-fd))

(defun make-unix-datagram (socket-fd &optional pathname)
  (make-instance 'unix-datagram
                 :socket socket-fd
                 :pathname pathname))

(defmethod close-datagram ((socket socket-datagram))
  (close-socket (socket-datagram-socket socket)))

(defmethod close-datagram :after ((socket socket-datagram))
  (setf (socket-open-p socket) nil))

(defmethod close-datagram :after ((socket unix-datagram))
  (ignore-errors
    (delete-file (unix-datagram-pathname socket))))

;; Register a special free action for closing datagram usocket when being GCed
(defun socket-special-free-action (object)
  (when (and (typep object 'socket-datagram)
             (socket-open-p object))
    (close-datagram object)))

(eval-when (:load-toplevel :execute)
  (hcl:add-special-free-action 'socket-special-free-action))

(defmethod get-socket-receive-timeout ((socket socket-datagram))
  (get-socket-receive-timeout (socket-datagram-socket socket)))

(defmethod set-socket-receive-timeout ((socket socket-datagram) seconds)
  (set-socket-receive-timeout (socket-datagram-socket socket) seconds))

(defgeneric send-message (socket buffer &key))

(defgeneric receive-message (socket &key))
