(in-package #:cl-zfs-backup.endpoint)

(defclass remote-target (target)
  ((%address
    :reader address
    :initarg :address)
   (%port
    :reader port
    :initarg :port)
   (%connectable?
    :accessor %connectable?
    :initform t)))

(defmethod cmd:run :around ((object remote-target) output (command list))
  (let ((prefix (cmd:make-remote-prefix (user object) (port object) (address object))))
    (call-next-method object output (nconc prefix command))))

(defun connectable? (endpoint)
  (let ((connectable? (cmd:? (endpoint) "exit")))
    (setf (connectable? endpoint) connectable?)
    connectable?))

(defgeneric (setf connectable?) (value target))

(defmethod (setf connectable?) ((value t) (target remote-target))
  (unless (%connectable? target)
    (let ((source-name (name (source target))))
      (r:log (:notice :collect :target-reconnected) source-name (hostname target))))
  (update-user/groups/hostname target)
  (setf (%connectable? target) t
        (available? target) t))

(defmethod (setf connectable?) ((value null) (target remote-target))
  (let ((source-name (name (source target)))
        (target-name (name target))
        (hostname (hostname target)))
    (r:log (:warn :collect :target-unconnectable) source-name target-name hostname)
    (setf (%connectable? target) nil
          (available? target) nil)))
