(in-package #:cl-user)

(defpackage #:cl-zfs-backup.dataset
  (:local-nicknames
   (#:cmd #:cl-zfs-backup.command)
   (#:lt #:local-time)
   (#:ss #:split-sequence)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:export
   #:+prefix+
   #:+prefix-end+
   #:bytes-referenced
   #:bytes-written
   #:common-snapshot
   #:container
   #:datasets
   #:destroy
   #:endpoint
   #:ensure-dataset
   #:exists?
   #:filesystem
   #:holds
   #:list-properties
   #:make-dataset
   #:name
   #:snapshot-order
   #:parent
   #:recent-snapshot
   #:snapshot
   #:source-filesystem
   #:suffix
   #:target-filesystem
   #:timestamp
   #:update
   #:update-container))
