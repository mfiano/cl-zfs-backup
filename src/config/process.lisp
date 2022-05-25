(in-package #:cl-zfs-backup.config)

(u:define-constant +valid-process-options+ '(:sources :interval :terminal-log-level :file-log-level)
  :test #'equal)

(defmethod check ((type (eql :process-sources)) value &key)
  (unless (and (listp value)
               (every (lambda (x) (typep x 'u:non-null-symbol)) value))
    (r:log (:error :config :process-invalid-sources)))
  (dolist (x value)
    (unless (u:href (sources *options*) x)
      (r:log (:error :config :process-undefined-source) x))))

(defmethod check ((type (eql :process-interval)) value &key)
  (unless value
    (r:log (:error :config :missing-key) :process :interval))
  (unless (typep value 'u:positive-integer)
    (r:log (:error :config :process-invalid-interval))))

(defmethod check ((type (eql :process-log-level)) value &key option)
  (let ((levels (r:get-log-levels)))
    (when (and value (not (member value levels)))
      (r:log (:error :config :process-invalid-log-level) option levels))))

(defun make-process-options (form)
  (check :valid-plist form :form-type :process)
  (check :allowed-keys form :form-type :process :valid +valid-process-options+)
  (check :duplicate-keys form :form-type :process)
  (destructuring-bind (&key sources interval terminal-log-level file-log-level) form
    (check :process-interval interval)
    (check :process-sources sources)
    (check :process-log-level terminal-log-level :option :terminal-log-level)
    (check :process-log-level file-log-level :option :file-log-level)
    (list :sources (mapcar (lambda (x) (u:href (sources *options*) x)) sources)
          :interval (* interval 60)
          :terminal-log-level (or terminal-log-level :info)
          :file-log-level (or file-log-level :debug))))

(defmacro define-process (() &body body)
  `(reinitialize-instance *options* :process (make-process-options ',@body)))
