(in-package :cl-user)

(require "comm")
;; Don't warn when I'm hacking the COMM package
(setf hcl:*packages-for-warn-on-redefinition*
      (remove "COMM" hcl:*packages-for-warn-on-redefinition* :test #'equal))

(defsystem lispworks-udp
  (:package :comm
   :optimize ((safety 3) (debug 3)))
  :members (lispworks-udp
            udp-client
            udp-server
            ; rtt
            ; unix
            )
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))))
