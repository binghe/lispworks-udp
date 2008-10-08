;;;; -*- Mode: Lisp -*-
;;;; $Id$

(in-package :comm+)

(defclass socket-datagram (rtt-info-mixin)
  ((open-p :type boolean
           :accessor socket-open-p
           :initform t)
   (socket :type integer
           :reader socket-datagram-socket
           :initarg :socket))
  (:documentation "datagram socket class"))

(defun make-datagram (socket-fd)
  (make-instance 'socket-datagram :socket socket-fd))

(defun close-datagram (socket)
  (declare (type socket-datagram socket))
  (setf (socket-open-p socket) nil)
  (close-socket (socket-datagram-socket socket)))

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
