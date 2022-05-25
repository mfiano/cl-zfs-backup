(in-package #:cl-user)

(defpackage #:cl-zfs-backup.config
  (:local-nicknames
   (#:r #:cl-zfs-backup.reporter)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:shadow
   #:load)
  (:export
   #:+pruning-buckets+
   #:load))
