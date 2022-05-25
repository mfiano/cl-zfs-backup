(in-package #:cl-zfs-backup.endpoint)

(defclass source (endpoint)
  ((%recursive
    :reader recursive?
    :initarg :recursive
    :initform nil)
   (%exclude
    :reader exclude
    :initarg :exclude
    :initform nil)
   (%minimum-size
    :reader minimum-size
    :initarg :minimum-size)
   (%targets
    :reader targets
    :initarg :targets)))

(defun make-source (options)
  (apply #'make-instance 'source options))

(defun make-targets (source)
  (let ((targets (mapcar
                  (lambda (x)
                    (destructuring-bind (mode . initargs) x
                      (load-target source mode initargs)))
                  (targets source))))
    (reinitialize-instance source :targets targets)))

(defmethod initialize-instance :after ((instance source) &key)
  (make-targets instance)
  (update-user/groups/hostname instance)
  (u:prependf (required-permissions instance) "send" "snapshot"))

(defmethod emit-base-path-warning ((endpoint source) (filesystem ds:source-filesystem))
  (r:log (:warn :collect :source-missing-base-path) (name endpoint) (ds:name filesystem)))

(defmethod emit-permission-warning ((endpoint source)
                                    (filesystem ds:source-filesystem)
                                    (permissions list))
  (r:log (:warn :collect :source-missing-permissions)
    (name endpoint) (ds:name filesystem) permissions))

(defun verify-targets (endpoint)
  (let ((availabilities nil))
    (dolist (target (targets endpoint))
      (let ((connectable? (connectable? target)))
        (when connectable?
          (push (verify-base-path target) availabilities))))
    (setf (available? endpoint) (some #'identity availabilities))))

(defmethod ds:update-container ((container source))
  (let ((excluded (exclude container)))
    (dolist (properties (ds:list-properties container :recursive? (recursive? container)))
      (destructuring-bind (name . data) properties
        (unless (find name excluded :test #'u:string-starts-with-p)
          (let ((filesystem (ds:ensure-dataset container name)))
            (ds:update filesystem data)))))
    (dolist (target (targets container))
      (when (available? target)
        (ds:update-container target)))))

(defmethod ds:update-container :around ((container source))
  (when (and (verify-base-path container)
             (verify-targets container))
    (let ((name (name container)))
      (r:log (:debug :collect :source-begin) name)
      (call-next-method)
      (r:log (:debug :collect :source-end) name))))
