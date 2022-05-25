(in-package #:cl-zfs-backup.reporter)

(define-condition clzb-error (simple-error) ())

(defun clzb-error (message &rest args)
  (error 'clzb-error :format-control message :format-arguments args))

(defun quit (&optional (code 1))
  (uiop:quit code))

(defun clzb-error-handler (condition)
  (declare (ignore condition))
  (quit))

(defun fatal-error-handler (condition)
  (log (:fatal :process :fatal-error) (dissect:present condition nil))
  (quit))

(defun user-abort-handler (condition)
  (declare (ignore condition))
  (log (:warn :process :user-abort-begin)))

(defmacro with-user-abort ((aborted?) &body body)
  (u:with-gensyms (previous-hook)
    (let ((hook #+sbcl 'sb-ext:*invoke-debugger-hook*
                #-sbcl *debugger-hook*))
      `(let* ((,aborted? nil)
              (,previous-hook ,hook)
              (,hook (lambda (condition hook)
                       (declare (ignore hook))
                       (typecase condition
                         #+sbcl
                         (sb-sys:interactive-interrupt
                          (setf ,aborted? t)
                          (continue))
                         (t
                          (let ((,hook ,previous-hook))
                            (invoke-debugger condition)))))))
         ,@body))))
