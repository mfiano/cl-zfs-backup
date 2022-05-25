(in-package #:cl-user)

(defpackage #:cl-zfs-backup.reporter
  (:local-nicknames
   (#:lt #:local-time)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:shadow
   #:log)
  (:export
   #:clzb-error
   #:clzb-error-handler
   #:fatal-error-handler
   #:get-log-levels
   #:log
   #:quit
   #:setup-logging
   #:user-abort-handler
   #:with-user-abort))
