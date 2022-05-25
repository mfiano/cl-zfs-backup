(in-package #:cl-user)

(defpackage #:cl-zfs-backup.command
  (:local-nicknames
   (#:r #:cl-zfs-backup.reporter)
   (#:ss #:split-sequence)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:export
   #:$
   #:?
   #:!
   #:make-remote-prefix
   #:run))
