(in-package #:cl-zfs-backup.endpoint)

(defclass endpoint (ds:container)
  ((%name
    :reader name
    :initarg :name)
   (%user
    :reader user
    :initarg :user)
   (%groups
    :reader groups
    :initarg :groups)
   (%hostname
    :reader hostname
    :initarg :hostname)
   (%base-path
    :reader base-path
    :initarg :base-path)
   (%policy
    :reader policy
    :initarg :policy)
   (%required-permissions
    :accessor required-permissions
    :initform '("destroy" "hold" "mount" "release"))
   (%available?
    :accessor available?
    :initform t)))

(u:define-printer (endpoint stream :identity nil)
  (format stream "~a, path: ~a" (name endpoint) (base-path endpoint)))

(defun update-user/groups/hostname (endpoint)
  (reinitialize-instance endpoint
                         :user (cmd:$ (endpoint) "id -un")
                         :groups (ss:split-sequence #\space (cmd:$ (endpoint) "id -Gn"))
                         :hostname (cmd:$ (endpoint) "uname -n")))

(defgeneric emit-base-path-warning (endpoint filesystem))

(defgeneric emit-permission-warning (endpoint filesystem permissions))

(defmethod ds:list-properties ((container endpoint) &key recursive?)
  (cmd:$ (container :table t)
    "zfs list -H -p -t filesystem -o name,refer,written"
    (when recursive? "-r")
    (base-path container)))

(defmethod ds:make-dataset ((container endpoint) (name string))
  (make-instance 'ds:filesystem :name name :endpoint container))

(defun permission? (endpoint filesystem)
  (u:mvlet* ((permissions (required-permissions endpoint))
             (users groups (pp:parse filesystem)))
    (flet ((check (table key)
             (setf permissions (set-difference permissions (u:href table key) :test #'string=))
             (unless permissions
               (return-from permission? t))))
      (check users (user endpoint))
      (dolist (group (groups endpoint))
        (check groups group))
      (emit-permission-warning endpoint filesystem permissions)
      nil)))

(defun verify-base-path (endpoint)
  (u:mvlet* ((base-path (base-path endpoint))
             (filesystem (ds:make-dataset endpoint base-path))
             (exists? (ds:exists? filesystem)))
    (unless exists?
      (emit-base-path-warning endpoint filesystem))
    (setf (available? endpoint) (and exists? (permission? endpoint filesystem)))))
