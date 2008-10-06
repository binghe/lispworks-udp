;;;; -*- Mode: Lisp -*-
;;;; $Id$

(in-package :comm+)

(defun default-rtt-function (message)
  (values message 0))

(defun sync-message (socket message &optional host service
                                    &key (max-receive-length +max-udp-message-size+)
                                         (encode-function #'default-rtt-function)
                                         (decode-function #'default-rtt-function))
  (let ((socket-fd (socket-datagram-socket socket)))
    (rtt-newpack socket)
    (multiple-value-bind (data send-seq) (funcall encode-function message)
      (let ((data-length (length data)))
        (loop
         with send-ts = (rtt-ts socket)
         and recv-message = nil
         and recv-seq = -1
         and continue-p = t
         do (progn
              (send-message socket data data-length host service)
              (loop with timeout-p = nil
                    do (progn
                         (set-socket-receive-timeout socket-fd (rtt-start socket))
                         (setf timeout-p nil)
                         (let ((m (receive-message socket nil nil
                                                   :max-buffer-size max-receive-length)))
                           (if m ; got a receive message
                             (multiple-value-setq (recv-message recv-seq)
                                 (funcall decode-function m))
                             ;; timeout
                             (let ((old-rto (slot-value socket 'rto)))
                               (setf continue-p (rtt-timeout socket)
                                     timeout-p t)
                               (warn 'rtt-timeout-warning
                                     :socket socket
                                     :old-rto old-rto
                                     :new-rto (slot-value socket 'rto))
                               (unless continue-p
                                 (error 'rtt-timeout-error :socket socket)
                                 (rtt-init socket))))))
                    until (and (not timeout-p)
                               (or (= recv-seq send-seq)
                                   (warn 'rtt-seq-mismatch-warning
                                         :socket socket
                                         :send-seq send-seq
                                         :recv-seq recv-seq)))
                    finally (let ((recv-ts (rtt-ts socket)))
                              (rtt-stop socket (- recv-ts send-ts))
                              (return nil))))
         until (or recv-message (not continue-p))
         finally (return recv-message))))))
