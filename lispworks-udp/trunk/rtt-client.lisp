;;;; -*- Mode: Lisp -*-
;;;; $Id$

(in-package :comm)

(defvar rtt-table (make-hash-table))

(defun default-rtt-function (message)
  (values message 0))

(defun sync-message (socket message host service
                            &key (max-receive-length +max-udp-message-size+)
                                 (encode-function #'default-rtt-function)
                                 (decode-function #'default-rtt-function))
  (let ((rtt-info (or nil ;(gethash socket rtt-table)
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
              (loop with timeout-p = nil
                    do (progn
                         (set-socket-receive-timeout socket (rtt-start rtt-info))
                         (format t "socket-timeout: ~A~%"
                                 (get-socket-receive-timeout socket))
                         (setf timeout-p nil)
                         (let ((m (receive-message socket nil max-receive-length)))
                           (if m ; got a receive message
                               (multiple-value-setq (recv-message recv-seq)
                                   (funcall decode-function m))
                             ;; timeout
                             (let ((old-rto (slot-value rtt-info 'rto)))
                               (setf continue-p (rtt-timeout rtt-info)
                                     timeout-p t)
                               (warn 'rtt-timeout-warning
                                     :old-rto old-rto
                                     :new-rto (slot-value rtt-info 'rto))
                               (unless continue-p
                                 (error 'rtt-timeout-error)
                                 (rtt-init rtt-info))))))
                    until (and (not timeout-p)
                               (or (= recv-seq send-seq)
                                   (warn 'rtt-seq-mismatch-warning
                                         :send-seq send-seq
                                         :recv-seq recv-seq)))
                    finally (let ((recv-ts (rtt-ts rtt-info)))
                              (rtt-stop rtt-info (- recv-ts send-ts))
                              (return nil))))
	 until (or recv-message (not continue-p))
	 finally (return recv-message))))))
