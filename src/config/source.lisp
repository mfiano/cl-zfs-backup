(in-package #:cl-zfs-backup.config)

(u:define-constant +valid-source-options+
    '(:base-path :recursive :exclude :minimum-size :policy :targets)
  :test #'equal)

(u:define-constant +valid-size-units+ '(:b :k :m :g) :test #'equal)

(defmethod check ((type (eql :source-base-path)) value &key name)
  (let ((base-path (getf value :base-path)))
    (unless base-path
      (r:log (:error :config :missing-key/named) :source name :base-path))
    (unless (stringp base-path)
      (r:log (:error :config :invalid-string/named) :source name :base-path))))

(defmethod check ((type (eql :source-recursive)) value &key name)
  (let ((recursive (getf value :recursive)))
    (unless (typep recursive 'boolean)
      (r:log (:error :config :invalid-boolean/named) :source name :recursive))))

(defmethod check ((type (eql :source-exclude)) value &key name)
  (let ((exclude (getf value :exclude)))
    (when (and (null (getf value :recursive)) exclude)
      (r:log (:error :config :source-invalid-exclude-use) name))
    (unless (and (listp exclude)
                 (every #'stringp exclude))
      (r:log (:error :config :source-invalid-exclude) name))
    (dolist (x exclude)
      (when (or (u:string-starts-with-p x "/")
                (u:string-ends-with-p x "/"))
        (r:log (:error :config :source-invalid-exclude-path) name)))))

(defmethod check ((type (eql :source-minimum-size)) value &key name)
  (destructuring-bind (&optional size-value size-unit) (u:ensure-list (getf value :minimum-size))
    (unless (and (typep size-value 'u:non-negative-integer)
                 (member size-unit +valid-size-units+))
      (r:log (:error :config :source-invalid-minimum-size) name +valid-size-units+))))

(defmethod check ((type (eql :source-policy)) value &key name)
  (let ((policy (getf value :policy)))
    (when (and policy (not (u:href (policies *options*) policy)))
      (r:log (:error :config :source-undefined-policy) name policy))))

(defmethod check ((type (eql :source-targets)) value &key name)
  (let ((targets (getf value :targets)))
    (unless (listp targets)
      (r:log (:error :config :source-invalid-targets) name))
    (dolist (target-form targets)
      (destructuring-bind (&optional target policy-key policy) (u:ensure-list target-form)
        (unless (and (typep target 'u:non-null-symbol)
                     (eq policy-key :policy)
                     (typep policy 'u:non-null-symbol))
          (r:log (:error :config :source-invalid-target) name target-form))
        (unless (u:href (targets *options*) target)
          (r:log (:error :config :source-undefined-target) name target))
        (unless (u:href (policies *options*) policy)
          (r:log (:error :config :source-undefined-target-policy) name target policy))))))

(defun check-source-options (name form)
  (check :valid-name name :form-type :source)
  (check :valid-plist form :form-type :source :name name)
  (check :allowed-keys form :form-type :source :name name :valid +valid-source-options+)
  (check :duplicate-keys form :form-type :source :name name :valid +valid-source-options+)
  (check :source-base-path form :name name)
  (check :valid-boolean (getf form :recursive) :form-type :source :name name :option :recursive)
  (check :source-recursive form :name name)
  (check :source-exclude form :name name)
  (check :source-minimum-size form :name name)
  (check :source-policy form :name name)
  (check :source-targets form :name name))

(defun make-source-target (target-form)
  (destructuring-bind (target &key policy) target-form
    (let ((target-options (u:href (targets *options*) target)))
      (list* (getf target-options :mode)
             :policy (u:href (policies *options*) policy)
             target-options))))

(defun make-source-options (name form)
  (check-source-options name form)
  (destructuring-bind (&key base-path recursive exclude minimum-size policy targets) form
    (destructuring-bind (size-value size-unit) minimum-size
      (let ((exclude (mapcar (lambda (x) (format nil "~a/~a" base-path x)) exclude))
            (minimum-size (from-bytes size-value size-unit))
            (targets (mapcar #'make-source-target targets)))
        (list :name name
              :base-path base-path
              :recursive recursive
              :exclude exclude
              :minimum-size minimum-size
              :policy (u:href (policies *options*) policy)
              :targets targets)))))

(defmacro define-source (name () &body body)
  `(setf (u:href (sources *options*) ',name) (make-source-options ',name ',@body)))
