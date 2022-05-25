(in-package #:cl-zfs-backup)

(defclass process ()
  ((%sources
    :reader sources
    :initarg :sources)
   (%interval
    :reader interval
    :initarg :interval)
   (%start-time
    :accessor start-time
    :initform (get-internal-real-time))
   (%elapsed-time
    :accessor elapsed-time
    :initform 0)
   (%periodic-time
    :accessor periodic-time
    :initform 0)))

(defmethod initialize-instance :after ((instance process)
                                       &key sources terminal-log-level file-log-level)
  (reinitialize-instance instance :sources (mapcar #'ep:make-source sources))
  (r:setup-logging terminal-log-level file-log-level)
  (start instance))

(defun make-process ()
  (apply #'make-instance 'process (cfg:load)))

(defun step? (process)
  (symbol-macrolet ((periodic-time (periodic-time process))
                    (elapsed-time (elapsed-time process)))
    (let ((current-time (/ (- (get-internal-real-time) (start-time process))
                           internal-time-units-per-second
                           1f0)))
      (incf periodic-time (- current-time elapsed-time))
      (setf elapsed-time current-time)
      (sleep 0.5f0)
      (when (or (>= periodic-time (interval process))
                (< periodic-time 0.5f0))
        (setf periodic-time 0f0)
        t))))

(defun step (process)
  (let ((sources (sources process)))
    (r:log (:info :process :cycle-begin))
    (p:collect-datasets sources)
    (p:take-snapshots sources)
    (p:replicate sources)
    (p:prune sources)
    (r:log (:info :process :cycle-end))))

(defun stop (process)
  (declare (ignore process))
  (r:log (:info :process :user-abort-end))
  (r:quit 130))

(defun start (process)
  (check-external-applications (sources process))
  (r:with-user-abort (aborted?)
    (u:until aborted?
      (when (step? process)
        (step process)))
    (stop process)))
