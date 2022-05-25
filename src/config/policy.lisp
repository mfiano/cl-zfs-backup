(in-package #:cl-zfs-backup.config)

(u:define-constant +pruning-buckets+ '(:minutes :hours :days :weeks :months :years) :test #'equal)

(defmethod check ((type (eql :policy-value)) value &key name key)
  (unless (typep value 'u:positive-integer)
    (r:log (:error :config :policy-invalid-value) name key)))

(defun check-policy-buckets (name form)
  (check :valid-name name :form-type :policy)
  (check :valid-plist form :form-type :policy :name name)
  (check :allowed-keys form :form-type :policy :name name :valid (cons :last +pruning-buckets+))
  (check :duplicate-keys form :form-type :policy :name name)
  (u:do-plist (key value form)
    (check :policy-value value :name name :key key))
  form)

(defmacro define-policy (name () &body body)
  `(setf (u:href (policies *options*) ',name) (check-policy-buckets ',name ',@body)))
