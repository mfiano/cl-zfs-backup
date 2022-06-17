(in-package #:cl-zfs-backup.endpoint)

(defclass target (endpoint)
  ((%mode
    :reader mode
    :initarg :mode)
   (%source
    :reader source
    :initarg :source)
   (%destructive?
    :reader destructive?
    :initarg :destructive
    :initform nil)))

(defmethod initialize-instance :after ((instance target) &key)
  (u:prependf (required-permissions instance) "canmount" "create" "receive"))

(defgeneric load-target (source mode options)
  (:method :around ((source source) (mode symbol) (options list))
    (apply #'make-instance (call-next-method) :source source options))
  (:method ((source source) (mode (eql :local)) (options list))
    'local-target)
  (:method ((source source) (mode (eql :ssh)) (options list))
    'remote-target))

(defmethod emit-base-path-warning ((endpoint target) (filesystem ds:target-filesystem))
  (let ((source-name (name (source endpoint))))
    (r:log (:warn :collect :target-missing-base-path)
      source-name (name endpoint) (hostname endpoint) (ds:name filesystem))))

(defmethod emit-permission-warning ((endpoint target)
                                    (filesystem ds:target-filesystem)
                                    (permissions list))
  (let ((source-name (name (source endpoint))))
    (r:log (:warn :collect :target-missing-permissions)
      source-name (name endpoint) (hostname endpoint) (ds:name filesystem) permissions)))

(defmethod ds:make-dataset ((container target) (name string))
  (make-instance 'ds:target-filesystem :name name :endpoint container))

(defmethod ds:update-container ((container target))
  (let* ((base-path (base-path container))
         (source (source container))
         (source-filesystems (ds:datasets source))
         (prefix (concatenate 'string (hostname source) "/"))
         (offset (+ (length base-path) (length prefix) 1))
         (relative-paths (mapcar (lambda (x) (concatenate 'string prefix x))
                                 (u:hash-keys source-filesystems))))
    (dolist (path relative-paths)
      (let ((filesystem (ds:ensure-dataset container (format nil "~a/~a" base-path path))))
        (unless (ds:exists? filesystem)
          (cmd:! (container) "zfs create -u -o canmount=noauto" (ds:name filesystem)))))
    (dolist (properties (ds:list-properties container :recursive? t))
      (destructuring-bind (name . data) properties
        (when (find name relative-paths :test #'u:string-ends-with-p)
          (let ((filesystem (ds:ensure-dataset container name))
                (source-filesystem (u:href source-filesystems (subseq name offset))))
            (setf (ds:source-filesystem filesystem) source-filesystem)
            (ds:update filesystem data)))))))

(defmethod ds:update-container :around ((container target))
  (let ((name (name container)))
    (r:log (:debug :collect :target-begin) name)
    (call-next-method)
    (r:log (:debug :collect :target-end) name)))
