(in-package #:cl-zfs-backup.command)

(defun make-remote-prefix (user port address)
  (list "ssh -q -o ControlMaster=auto -o ControlPath='/tmp/cl-zfs-backup_%r@%h:%p'"
        "-o ControlPersist=60 -o BatchMode=yes -l" user "-p" port address))

(defun %run (command &rest args)
  (let ((*error-output* (make-broadcast-stream))
        (command (format nil "~{~a~^ ~}" (remove nil command))))
    (r:log (:trace :command :trace) command)
    (handler-case (apply #'uiop:run-program command args)
      (uiop/run-program:subprocess-error (condition)
        (signal condition)
        (r:log (:error :command :failed)
          (uiop:subprocess-error-command condition)
          (uiop:subprocess-error-code condition))))))

(defmacro $ ((endpoint &key table) &body body)
  `(run ,endpoint (if ,table :list :string) (list ,@body)))

(defmacro ? ((endpoint) &body body)
  `(run ,endpoint :test (list ,@body)))

(defmacro ! ((endpoint) &body body)
  `(run ,endpoint nil (list ,@body)))

(defgeneric run (object output command))

(defmethod run (object output (command string))
  (run object output (list command)))

(defmethod run (object (output (eql :string)) (command list))
  (values (%run command :output '(:string :stripped t))))

(defmethod run (object (output (eql :list)) (command list))
  (with-input-from-string (in (%run command :output '(:string :stripped t)))
    (mapcar (lambda (x) (ss:split-sequence #\tab x)) (uiop:slurp-stream-lines in))))

(defmethod run (object (output null) (command list))
  (values (%run command :output nil)))

(defmethod run (object (output (eql :test)) (command list))
  (u:mvlet ((output error-output exit-code (%run command :ignore-error-status t)))
    (values (when (zerop exit-code) t)
            exit-code)))
