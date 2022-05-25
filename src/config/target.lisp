(in-package #:cl-zfs-backup.config)

(u:define-constant +valid-target-modes+ '(:local :ssh) :test #'equal)

(u:define-constant +valid-target-ssh-options+ '(:address :user :port :base-path :destructive)
  :test #'equal)

(defmethod check ((type (eql :target-mode)) value &key name form)
  (unless (member value +valid-target-modes+)
    (r:log (:error :config :target-invalid-mode) name +valid-target-modes+))
  (ecase value
    (:local
      (check :allowed-keys form :form-type :target :name name :valid '(:base-path)))
    (:ssh
     (check :allowed-keys form :form-type :target :name name :valid +valid-target-ssh-options+)
     (destructuring-bind (&key user address port &allow-other-keys) form
       (check :valid-string user :form-type :target :name name :option :user)
       (check :valid-string address :form-type :target :name name :option :address)
       (unless (typep port '(integer 1 65535))
         (r:log (:error :config :target-invalid-port) name))))))

(defmethod check ((type (eql :target-base-path)) value &key name)
  (let ((base-path (getf value :base-path)))
    (unless base-path
      (r:log (:error :config :missing-key/named) :target name :base-path))
    (unless (stringp base-path)
      (r:log (:error :config :invalid-string/named) :target name :base-path))
    (when (or (u:string-starts-with-p base-path "/")
              (u:string-ends-with-p base-path "/"))
      (r:log (:error :config :target-invalid-base-path) name))))

(defun make-target-options (name mode form)
  (check :valid-name name :form-type :target)
  (check :valid-plist form :form-type :target :name name)
  (check :target-mode mode :name name :form form)
  (check :duplicate-keys form :form-type :target :name name)
  (check :valid-boolean (getf form :destructive) :form-type :target :name name :option :destructive)
  (check :target-base-path form :name name)
  (list* :name name :mode mode form))

(defmacro define-target (name (&key (mode 'local)) &body body)
  `(setf (u:href (targets *options*) ',name) (make-target-options ',name ',mode ',@body)))
