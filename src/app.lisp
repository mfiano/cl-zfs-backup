(in-package #:cl-zfs-backup)

(defun app ()
  (handler-bind ((r:clzb-error #'r:clzb-error-handler)
                 (wua:user-abort #'r:user-abort-handler)
                 (error #'r:fatal-error-handler))
    (wua:with-user-abort (make-process))))

(defun build (file &optional (level t))
  #+sbcl
  (progn
    (sb-ext:disable-debugger)
    (sb-ext:gc :full t)
    (sb-ext:save-lisp-and-die
     file
     :toplevel #'app
     :executable t
     :save-runtime-options t
     #+sb-core-compression :compression
     #+sb-core-compression level))
  #-sbcl
  (error "Building is only supported on SBCL."))
