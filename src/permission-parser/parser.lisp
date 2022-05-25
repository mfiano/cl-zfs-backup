(in-package #:cl-zfs-backup.permission-parser)

(defun tokenize (string)
  (u:mappend
   (lambda (x)
     (let ((trimmed (string-trim '(#\space #\tab) (string-trim '(#\-) x))))
       (u:mappend
        (lambda (x)
          (re:split "(,)" x :with-registers-p t :omit-unmatched-p t))
        (re:split "\\s+" trimmed))))
   (re:split "\\n" string)))

(defgeneric make-node (type tokens))

(defmethod make-node ((type (eql 'permissions)) tokens)
  (loop :for (permission . rest) :on tokens :by #'cddr
        :collect permission :into permissions
        :while (string= (first rest) ",")
        :finally (return (values permissions rest))))

(defmethod make-node ((type (eql 'entity)) tokens)
  (let ((entity-type (u:format-symbol :keyword "~:@(~a~)" (pop tokens)))
        (valid-tokens '(:user :group :everyone)))
    (unless (member entity-type valid-tokens)
      (r:log (:error :collect :permission-parse-failed) valid-tokens type))
    (if (eq entity-type :everyone)
        (values entity-type t tokens)
        (values entity-type (pop tokens) tokens))))

(defmethod make-node ((type (eql 'permission-spec)) tokens)
  (u:mvlet* ((entity-type entity-value tokens (make-node 'entity tokens))
             (permissions tokens (make-node 'permissions tokens)))
    (values (list :entity-type entity-type :entity-value entity-value :permissions permissions)
            tokens)))

(defmethod make-node ((type (eql 'permission-specs)) tokens)
  (if (and tokens (member (first tokens) '("user" "group" "everyone") :test #'string=))
      (u:mvlet* ((spec tokens (make-node 'permission-spec tokens))
                 (specs tokens (make-node 'permission-specs tokens)))
        (values (cons spec specs) tokens))
      (values nil tokens)))

(defmethod make-node ((type (eql 'scope)) tokens)
  (let ((scope (u:format-symbol :keyword "~:@(~a~)" (pop tokens)))
        (valid-tokens '(:local :descendent :local+descendent)))
    (unless (member scope valid-tokens)
      (r:log (:error :collect :permission-parse-failed) valid-tokens type))
    (values scope tokens)))

(defmethod make-node ((type (eql 'block)) tokens)
  (u:mvlet ((scope tokens (make-node 'scope tokens)))
    (unless (string= #1=(pop tokens) "permissions:")
      (r:log (:error :collect :permission-parse-failed) #1# type))
    (u:mvlet ((specs tokens (make-node 'permission-specs tokens)))
      (values (list :scope scope :specs specs) tokens))))

(defmethod make-node ((type (eql 'blocks)) tokens)
  (if (and tokens (string= (second tokens) "permissions:"))
      (u:mvlet* ((block tokens (make-node 'block tokens))
                 (blocks tokens (make-node 'blocks tokens)))
        (values (cons block blocks) tokens))
      (values nil tokens)))

(defmethod make-node ((type (eql 'filesystem)) tokens)
  (unless (string= #1=(pop tokens) "Permissions")
    (r:log (:error :collect :permission-parse-failed) #1# type))
  (unless (string= #2=(pop tokens) "on")
    (r:log (:error :collect :permission-parse-failed) #2# type))
  (u:mvlet ((filesystem (pop tokens))
            (blocks tokens (make-node 'blocks tokens)))
    (values (list :filesystem filesystem :blocks blocks) tokens)))

(defmethod make-node ((type (eql 'filesystems)) tokens)
  (when (and tokens (equal (first tokens) "Permissions"))
    (u:mvlet* ((filesystem tokens (make-node 'filesystem tokens))
               (filesystems tokens (make-node 'filesystems tokens)))
      (cons filesystem filesystems))))

(defun flatten-tree (tree)
  (let ((results nil))
    (dolist (x tree)
      (destructuring-bind (&key filesystem blocks) x
        (dolist (x blocks)
          (destructuring-bind (&key scope specs) x
            (dolist (x specs)
              (destructuring-bind (&key entity-type entity-value permissions) x
                (push (list filesystem scope entity-type entity-value :permissions permissions)
                      results)))))))
    results))

(defun filter (data filesystem)
  (remove-if
   (lambda (x)
     (destructuring-bind (name scope . rest) x
       (declare (ignore rest))
       (and (string/= name filesystem)
            (eq scope :local))))
   (nreverse data)))

(defun split (data filesystem)
  (u:partition (lambda (x) (string= x filesystem)) data :key #'car))

(defun post-process-tree (tree filesystem)
  (split (filter (flatten-tree tree) filesystem) filesystem))

(defun merge-parents (targets parents)
  (let ((users (u:dict #'equalp))
        (groups (u:dict #'equalp)))
    (labels ((%merge (table key permissions)
               (setf (u:href table key) (union (u:href table key) permissions :test #'string=)))
             (populate-tables (data)
               (dolist (x data)
                 (destructuring-bind (filesystem scope &key user group everyone permissions) x
                   (declare (ignore filesystem scope))
                   (when user
                     (%merge users user permissions))
                   (when group
                     (%merge groups group permissions))
                   (when everyone
                     (u:do-hash-keys (user users)
                       (%merge users user permissions)))))))
      (populate-tables targets)
      (populate-tables parents)
      (values users groups))))

(defun parse (filesystem)
  (u:mvlet* ((name (ds:name filesystem))
             (tokens (tokenize (cmd:$ ((ds:endpoint filesystem)) "zfs allow" name)))
             (tree (make-node 'filesystems tokens))
             (targets parents (post-process-tree tree name)))
    (merge-parents targets parents)))
