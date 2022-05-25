(in-package #:cl-zfs-backup.dataset)

(defclass filesystem (container dataset)
  ((%parent
    :reader parent
    :initarg :parent
    :initform nil)
   (%bytes-written
    :reader bytes-written
    :initarg :bytes-written)
   (%snapshot-order
    :accessor snapshot-order
    :initform nil)
   (%recent-snapshot
    :accessor recent-snapshot
    :initform nil)
   (%holds-fetched?
    :accessor holds-fetched?
    :initform nil)))

(defmethod initialize-instance :after ((instance filesystem) &key name endpoint)
  (u:when-let* ((parent-path-separator (position #\/ name :from-end t))
                (parent-path (subseq name 0 parent-path-separator)))
    (reinitialize-instance instance :parent (u:href (datasets endpoint) parent-path))))

(defmethod exists? ((dataset filesystem))
  (cmd:? ((endpoint dataset)) "zfs list" (name dataset)))

(defmethod list-properties ((container filesystem) &key)
  (cmd:$ ((endpoint container) :table t)
    "zfs list -H -p -t snapshot -S createtxg -o name,refer,creation"
    (name container)))

(defmethod make-dataset ((container filesystem) (name string))
  (make-instance 'snapshot :name name :endpoint (endpoint container)))

(defmethod update ((dataset filesystem) (data list))
  (destructuring-bind (referenced written) data
    (update-container dataset)
    (reinitialize-instance dataset
                           :bytes-referenced (parse-integer referenced)
                           :bytes-written (parse-integer written))))

(defmethod update-container ((container filesystem))
  (loop :with holds-fetched? := (holds-fetched? container)
        :with endpoint := (endpoint container)
        :for properties :in (list-properties container)
        :for (name . data) := properties
        :for i :from 0
        :for snapshot := (ensure-dataset container name)
        :do (update snapshot data)
        :when (zerop i)
          :do (setf (recent-snapshot container) snapshot)
        :unless holds-fetched?
          :collect name :into names
        :collect snapshot :into order
        :finally (setf (snapshot-order container) order)
                 (unless holds-fetched?
                   (dolist (batch (u:batches names 8))
                     (register-holds container batch))
                   (setf (holds-fetched? container) t))))
