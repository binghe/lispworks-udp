;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; Condition for LispWorks-UDP

(in-package :comm)

(define-condition rtt-timeout-warning (warning)
  ((old-rto :type short-float
            :reader old-rto-of
            :initarg :old-rto)
   (new-rto :type short-float
            :reader new-rto-of
            :initarg :new-rto))
  (:report (lambda (condition stream)
             (format stream "Receive timeout (~As), next: ~As.~%"
                     (old-rto-of condition)
                     (new-rto-of condition))))
  (:documentation "RTT timeout warning"))

(define-condition rtt-seq-mismatch-warning (warning)
  ((send-seq :type integer
             :reader send-seq-of
             :initarg :send-seq)
   (recv-seq :type integer
             :reader recv-seq-of
             :initarg :recv-seq))
  (:report (lambda (condition stream)
             (format stream "Sequence number mismatch (~A -> ~A), try read again.~%"
                     (send-seq-of condition)
                     (recv-seq-of condition))))
  (:documentation "RTT sequence mismatch warning"))

(define-condition rtt-timeout-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Max retransmit times (~A) reached, give up.~%"
                     *rtt-maxnrexmt*)))
  (:documentation "RTT timeout error"))
