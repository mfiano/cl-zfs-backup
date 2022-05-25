(in-package #:cl-zfs-backup)

(defun check-external-applications (sources)
  (let ((source (first sources))
        (errors nil)
        (seen-targets (u:dict #'eq)))
    (flet ((check (endpoint command name)
             (unless (cmd:? (endpoint) command)
               (push (format nil "`~a` not installed on host '~(~a~)'~%"
                             name (ep:hostname endpoint))
                     errors))))
      (check source "ssh -V" "OpenSSH")
      (check source "zfs --version" "ZFS")
      (dolist (source sources)
        (dolist (target (ep:targets source))
          (let ((target-name (ep:name target)))
            (unless (u:href seen-targets target-name)
              (check target "zfs --version" "ZFS"))
            (setf (u:href seen-targets target-name) t))))
      (when errors
        (r:log (:error :process :dependencies-not-found) errors)))))