(asdf:defsystem #:cl-zfs-backup
  :description "An automated ZFS filesystem backup solution."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/cl-zfs-backup"
  :version "1.0.0"
  :encoding :utf-8
  :depends-on
  (#:cl-ppcre
   #:dissect
   #:local-time
   #:mfiano-utils
   #:split-sequence
   #:uiop
   #:with-user-abort)
  :pathname "src"
  :serial t
  :components
  ((:module "reporter"
    :components
    ((:file "package")
     (:file "messages")
     (:file "logger")
     (:file "condition")))
   (:module "command"
    :components
    ((:file "package")
     (:file "command")))
   (:module "config"
    :components
    ((:file "package")
     (:file "common")
     (:file "policy")
     (:file "target")
     (:file "source")
     (:file "process")
     (:file "load")))
   (:module "dataset"
    :components
    ((:file "package")
     (:file "container")
     (:file "dataset")
     (:file "filesystem")
     (:file "source-filesystem")
     (:file "target-filesystem")
     (:file "snapshot")
     (:file "bookmark")))
   (:module "permission-parser"
    :components
    ((:file "package")
     (:file "parser")))
   (:module "endpoint"
    :components
    ((:file "package")
     (:file "endpoint")
     (:file "source")
     (:file "target")
     (:file "local-target")
     (:file "remote-target")))
   (:module "phases"
    :components
    ((:file "package")
     (:file "collect")
     (:file "snapshot")
     (:file "replicate")
     (:file "prune")))
   (:file "package")
   (:file "startup-check")
   (:file "process")
   (:file "app")))
