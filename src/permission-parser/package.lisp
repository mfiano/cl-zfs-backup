(in-package #:cl-user)

(defpackage #:cl-zfs-backup.permission-parser
  (:local-nicknames
   (#:cmd #:cl-zfs-backup.command)
   (#:ds #:cl-zfs-backup.dataset)
   (#:r #:cl-zfs-backup.reporter)
   (#:re #:cl-ppcre)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:export
   #:parse))
