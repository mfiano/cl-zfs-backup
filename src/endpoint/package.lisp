(in-package #:cl-user)

(defpackage #:cl-zfs-backup.endpoint
  (:local-nicknames
   (#:cmd #:cl-zfs-backup.command)
   (#:ds #:cl-zfs-backup.dataset)
   (#:lt #:local-time)
   (#:pp #:cl-zfs-backup.permission-parser)
   (#:r #:cl-zfs-backup.reporter)
   (#:ss #:split-sequence)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:export
   #:address
   #:destructive?
   #:endpoint
   #:hostname
   #:make-source
   #:minimum-size
   #:name
   #:policy
   #:port
   #:source
   #:target
   #:targets
   #:user))
