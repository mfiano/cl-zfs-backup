(in-package #:cl-zfs-backup.phases)

(defun collect-datasets (sources)
  (r:log (:info :collect :phase-begin))
  (dolist (source sources)
    (ds:update-container source))
  (r:log (:info :collect :phase-end)))
