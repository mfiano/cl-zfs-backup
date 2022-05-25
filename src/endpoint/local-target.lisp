(in-package #:cl-zfs-backup.endpoint)

(defclass local-target (target) ())

(defmethod initialize-instance :after ((instance local-target) &key)
  (update-user/groups/hostname instance))
