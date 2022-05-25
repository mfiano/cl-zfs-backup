(in-package #:cl-zfs-backup.dataset)

(defclass container ()
  ((%datasets
    :reader datasets
    :initform (u:dict #'equalp))
   (%updated-datasets
    :reader updated-datasets
    :initform (u:dict #'equalp))))

(defgeneric make-dataset (container name))

(defgeneric list-properties (container &key))

(defgeneric ensure-dataset (container name)
  (:method ((container container) (name string))
    (let ((datasets (datasets container)))
      (or (u:href datasets name)
          (setf (u:href datasets name) (make-dataset container name)))))
  (:method :after ((container container) (name string))
    (setf (u:href (updated-datasets container) name) t)))

(defgeneric update-container (container)
  (:method :before ((container container))
    (clrhash (updated-datasets container)))
  (:method :after ((container container))
    (let ((datasets (datasets container))
          (updated (updated-datasets container)))
      (u:do-hash-keys (name datasets)
        (unless (u:href updated name)
          (remhash name datasets))))))
