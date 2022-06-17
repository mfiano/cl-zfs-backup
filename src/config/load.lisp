(in-package #:cl-zfs-backup.config)

(defun find-file ()
  (let ((paths (list (namestring (uiop:merge-pathnames* "clzb/clzb.conf" (uiop:xdg-config-home)))
                     (namestring (uiop:merge-pathnames* ".clzb.conf" (user-homedir-pathname))))))
    (values (find-if #'uiop:file-exists-p paths)
            paths)))

(defun load ()
  (u:mvlet ((file paths (find-file)))
    (if file
        (let ((*package* (find-package :cl-zfs-backup.config)))
          (cl:load (uiop:ensure-pathname file) :verbose nil)
          (process *options*))
        (r:log (:error :config :file-not-found) paths))))
