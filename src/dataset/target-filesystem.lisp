(in-package #:cl-zfs-backup.dataset)

(defclass target-filesystem (filesystem)
  ((%source-filesystem
    :accessor source-filesystem
    :initform nil)
   (%common-snapshot
    :accessor common-snapshot
    :initform nil)))

(defmethod update-container :after ((container target-filesystem))
  (let ((table (u:dict #'equalp)))
    (dolist (snapshot (snapshot-order (source-filesystem container)))
      (setf (u:href table (suffix snapshot)) t))
    (u:when-let* ((suffix (find-if (lambda (x) (u:href table x))
                                   (mapcar #'suffix (snapshot-order container))))
                  (name (format nil "~a@~a" (name container) suffix)))
      (setf (common-snapshot container) (u:href (datasets container) name)))))
