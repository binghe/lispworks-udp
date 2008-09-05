;;;; -*- Mode: Lisp -*-
;;;; $Id$

(in-package :comm)

(defvar rtt-table (make-hash-table))

(defun default-rtt-function (message)
  (values message 0))

(defun sync-message (socket message host service
                            &key (max-buffer-size +max-udp-message-size+)
                                 (encode-function #'default-rtt-function)
                                 (decode-function #'default-rtt-function))
  (let ((rtt-info (and (gethash socket rtt-table)
                       (setf (gethash socket rtt-table)
                             (make-instance 'rtt-info-mixin)))))
  (rtt-newpack rtt-info)
  (multiple-value-bind (data send-seq) (funcall encode-function message)
    (let ((data-length (length data)))
      (loop
	 with send-ts = (rtt-ts rtt-info)
	 and recv-message = nil
	 and recv-seq = -1
	 and continue-p = t
	 do (progn
	      (send-message socket data data-length host service)
              (loop do (progn
                         (set-socket-receive-timeout socket (rtt-start rtt-info))
                         (let ((m (receive-message socket nil max-buffer-size)))
                           (if m ; got a receive message
                               (multiple-value-setq (recv-message recv-seq)
                                   (funcall decode-function m))
                             (progn ; timeout
                               (setf continue-p (rtt-timeout rtt-info))
                               (unless continue-p
                                 (rtt-init rtt-info))))))
                    until (= recv-seq send-seq)
                    finally (let ((recv-ts (rtt-ts rtt-info)))
                              (rtt-stop socket (- recv-ts send-ts))
                              (return nil))))
	 until (or recv-message (not continue-p))
	 finally (return recv-message))))))
