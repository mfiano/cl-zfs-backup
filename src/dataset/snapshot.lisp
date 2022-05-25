(in-package #:cl-zfs-backup.dataset)

(defclass snapshot (dataset)
  ((%filesystem
    :reader filesystem
    :initarg :filesystem)
   (%suffix
    :reader suffix
    :initarg :suffix)
   (%timestamp
    :reader timestamp
    :initarg :timestamp
    :initform nil)))

(defun register-holds (filesystem snapshot-names)
  (let ((filesystem-holds (holds filesystem))
        (snapshots (datasets filesystem))
        (table (cmd:$ ((endpoint filesystem) :table t)
                 "zfs holds -H" (format nil "~{~a~^ ~}" snapshot-names))))
    (dolist (row table)
      (destructuring-bind (name tag) (butlast row)
        (when (u:string-starts-with-p tag +prefix+)
          (let* ((snapshot (u:href snapshots name))
                 (snapshot-holds (holds snapshot))
                 (hostname (subseq tag +prefix-end+)))
            (setf (u:href snapshot-holds tag) t)
            (push snapshot (u:href filesystem-holds hostname))))))))

(defun held? (snapshot)
  (plusp (hash-table-count (holds snapshot))))

(defmethod initialize-instance :after ((instance snapshot) &key name endpoint)
  (destructuring-bind (prefix suffix) (ss:split-sequence #\@ name :count 2)
    (let ((filesystem (u:href (datasets endpoint) prefix)))
      (reinitialize-instance instance :filesystem filesystem :suffix suffix))))

(defmethod exists? ((dataset snapshot))
  (cmd:? ((endpoint dataset)) "zfs list -t snapshot" (name dataset)))

(defmethod update ((dataset snapshot) (data list))
  (destructuring-bind (referenced time) data
    (reinitialize-instance dataset
                           :timestamp (lt:unix-to-timestamp (parse-integer time))
                           :bytes-referenced (parse-integer referenced))))

(defmethod destroy ((dataset snapshot))
  (let* ((endpoint (endpoint dataset))
         (name (name dataset))
         (info (cmd:$ (endpoint :table t) "zfs destroy -npv" name)))
    (cmd:! (endpoint) "zfs destroy -d" name)
    (if (held? dataset) 0 (parse-integer (cadar (last info))))))
