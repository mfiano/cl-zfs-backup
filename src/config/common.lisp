(in-package #:cl-zfs-backup.config)

(defclass options ()
  ((%policies
    :reader policies
    :initform (u:dict #'eq))
   (%targets
    :reader targets
    :initform (u:dict #'eq))
   (%sources
    :reader sources
    :initform (u:dict #'eq))
   (%process
    :reader process
    :initarg :process)))

(defvar *options* (make-instance 'options))

(defgeneric check (type value &key form-type &allow-other-keys))

(defmethod check ((type (eql :valid-name)) value &key form-type)
  (unless (typep value 'u:non-null-symbol)
    (r:log (:error :config :invalid-name) form-type)))

(defmethod check ((type (eql :valid-plist)) value &key form-type name)
  (unless (u:plist-p value)
    (if name
        (r:log (:error :config :invalid-plist/named) form-type name)
        (r:log (:error :config :invalid-plist) form-type))))

(defmethod check ((type (eql :allowed-keys)) value &key form-type name valid)
  (let ((invalid-keys (u:plist-keys (apply #'u:plist-remove value valid))))
    (when invalid-keys
      (if name
          (r:log (:error :config :invalid-keys/named) form-type name invalid-keys)
          (r:log (:error :config :invalid-keys) form-type invalid-keys)))))

(defmethod check ((type (eql :duplicate-keys)) value &key form-type name)
  (flet ((find-duplicates (value)
           (let* ((hash-set (u:dict #'eq))
                  (keys (u:plist-keys value))
                  (duplicates nil))
             (dolist (x (remove-duplicates keys :test #'eq))
               (setf (u:href hash-set x) nil))
             (dolist (x keys)
               (if (u:href hash-set x)
                   (pushnew x duplicates :test #'eq)
                   (setf (u:href hash-set x) t)))
             (nreverse duplicates))))
    (u:when-let ((duplicates (find-duplicates value)))
      (if name
          (r:log (:error :config :duplicate-keys/named) form-type name value duplicates)
          (r:log (:error :config :duplicate-keys) form-type value duplicates)))))

(defmethod check ((type (eql :valid-string)) value &key form-type name option)
  (unless (stringp value)
    (if name
        (r:log (:error :config :invalid-string/named) form-type name option)
        (r:log (:error :config :invalid-string) form-type option))))

(defmethod check ((type (eql :valid-boolean)) value &key form-type name option)
  (unless (typep value 'boolean)
    (if name
        (r:log (:error :config :invalid-boolean/named) form-type name option)
        (r:log (:error :config :invalid-boolean) form-type option))))

(defun from-bytes (value to-unit)
  (ecase to-unit
    ((:b :bytes)
     (values value :b))
    ((:k :kib :kibibytes)
     (* value #.(expt 2 10)))
    ((:m :mib :mebibytes)
     (* value #.(expt 2 20)))
    ((:g :gib :gibibytes)
     (* value #.(expt 2 30)))
    ((:t :tib :tebibytes)
     (* value #.(expt 2 40)))
    ((:kb :kilobytes)
     (* value 1000))
    ((:mb :megabytes)
     (* value #.(expt 1000 2)))
    ((:gb :gigabytes)
     (* value #.(expt 1000 3)))
    ((:tb :terabytes)
     (* value #.(expt 1000 4)))))
