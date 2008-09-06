;;;; UNIX Network Programming v1 - Networking APIs: Sockets and XTI
;;;;  Chapter 20: Advance UDP Sockets
;;;;   Adding Reliability to a UDP Application

(in-package :comm)

(defclass rtt-info ()
  ((rtt    :accessor rtt-of
           :type short-float
           :initform 0.0
           :documentation "most recent measured RTT, seconds")
   (srtt   :accessor srtt-of
           :type short-float
           :initform 0.0
           :documentation "smoothed RTT estimator, seconds")
   (rttvar :accessor rttvar-of
           :type short-float
           :initform 0.75
           :documentation "smoothed mean deviation, seconds")
   (rto    :accessor rto-of
           :type short-float
           :documentation "current RTO to use, seconds")
   (nrexmt :accessor nrexmt-of
           :type fixnum
           :documentation "#times retransmitted: 0, 1, 2, ...")
   (base   :accessor base-of
           :type integer
           :documentation "#sec since 1/1/1970 at start, but we use Lisp time here"))
  (:documentation "RTT Info Class"))

(defvar *rtt-rxtmin*   2 "min retransmit timeout value, seconds")
(defvar *rtt-rxtmax*  60 "max retransmit timeout value, seconds")
(defvar *rtt-maxrexmt* 3 "max #times to retransmit")

(defmethod rtt-rtocalc ((instance rtt-info))
  "Calculate the RTO value based on current estimators:
        smoothed RTT plus four times the deviation."
  (with-slots (srtt rttvar) instance
    (+ srtt (* 4.0 rttvar))))

(defun rtt-minmax (rto)
  "rtt-minmax makes certain that the RTO is between the upper and lower limits."
  (declare (type short-float rto))
  (cond ((< rto *rtt-rxtmin*) *rtt-rxtmin*)
        ((> rto *rtt-rxtmax*) *rtt-rxtmax*)
        (t rto)))

(defmethod initialize-instance :after ((self rtt-info) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (base rto) self
    (setf base (get-internal-real-time)
          rto (rtt-minmax (rtt-rtocalc self)))))

(defmethod rtt-ts ((instance rtt-info))
  (* (- (get-internal-real-time) (base-of instance))
     #.(/ 1000 internal-time-units-per-second)))

(defmethod rtt-start ((instance rtt-info))
  "return value can be used as: alarm(rtt_start(&foo))"
  (round (rto-of instance)))

(defmethod rtt-stop ((instance rtt-info) (ms integer))
  (with-slots (rtt srtt rttvar rto) instance
    (setf rtt (/ ms 1000.0))
    (let ((delta (- rtt srtt)))
      (incf srtt (/ delta 8.0))
      (setf delta (abs delta))
      (incf rttvar (/ (- delta rttvar) 4.0)))
    (setf rto (rtt-minmax (rtt-rtocalc instance)))))

(defmethod rtt-timeout ((instance rtt-info))
  (with-slots (rto nrexmt) instance
    (setf rto (* rto 2))
    (<= (incf nrexmt) *rtt-maxrexmt*)))

(defmethod rtt-newpack ((instance rtt-info))
  (setf (nrexmt-of instance) 0))
