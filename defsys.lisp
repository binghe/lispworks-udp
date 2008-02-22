(in-package :cl-user)

(defsystem lispworks-udp
  (:package :comm
   :optimize ((safety 3) (debug 3)))
  :members (lispworks-udp
            rtt)
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))))
