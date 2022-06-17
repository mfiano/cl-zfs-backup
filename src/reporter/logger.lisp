(in-package #:cl-zfs-backup.reporter)

(u:define-constant +levels+
    '(:trace -20 :debug -10 :info 0 :notice 10 :warn 20 :error 30 :fatal 40) :test #'equal)

(u:define-constant +date-format+ '((:year 4) #\- (:month 2) #\- (:day 2)) :test #'equal)

(u:define-constant +time-format+ '((:hour 2) #\: (:min 2) #\: (:sec 2)) :test #'equal)

(defvar *terminal-log-level* :info)

(defvar *file-log-level* :debug)

(u:eval-always
  (defun get-log-path ()
    (let ((path (uiop:merge-pathnames* "clzb/clzb.log" (uiop:xdg-data-home))))
      (ensure-directories-exist path)
      (namestring path))))

(defun get-log-levels ()
  (u:plist-keys +levels+))

(defmacro log ((level category type) &body args)
  (u:with-gensyms (timestamp date time string stream)
    (let ((prefix (format nil "~(~6a~) | ~(~9a~)" level category))
          (message (message level category type)))
      `(let* ((*print-pretty* nil)
              (,timestamp (lt:now))
              (,date (lt:format-timestring nil ,timestamp :format +date-format+))
              (,time (lt:format-timestring nil ,timestamp :format +time-format+))
              (,string (format nil "~a | ~a | ~a | ~a~%" ,date ,time ,prefix ,message)))
         (when (>= ,(getf +levels+ level) (getf +levels+ *terminal-log-level*))
           (format t ,string ,@args))
         (when (>= ,(getf +levels+ level) (getf +levels+ *file-log-level*))
           (u:with-file-output (,stream ,(get-log-path) t)
             (format ,stream ,string ,@args)))
         ,@(when (member level '(:error :fatal))
             `((clzb-error ,message ,@args)))
         (values)))))

(defun setup-logging (terminal-level file-level)
  (when terminal-level
    (setf *terminal-log-level* terminal-level))
  (when file-level
    (setf *file-log-level* file-level))
  (log (:info :process :file-logging) (get-log-path)))
