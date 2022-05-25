(in-package #:cl-zfs-backup.dataset)

(defclass bookmark (dataset)
  ((%filesystem
    :reader filesystem
    :initarg :filesystem)))
