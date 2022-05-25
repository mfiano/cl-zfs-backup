(in-package #:cl-user)

(defpackage #:cl-zfs-backup.phases
  (:local-nicknames
   (#:cfg #:cl-zfs-backup.config)
   (#:cmd #:cl-zfs-backup.command)
   (#:ds #:cl-zfs-backup.dataset)
   (#:ep #:cl-zfs-backup.endpoint)
   (#:lt #:local-time)
   (#:r #:cl-zfs-backup.reporter)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:export
   #:collect-datasets
   #:prune
   #:replicate
   #:take-snapshots))
