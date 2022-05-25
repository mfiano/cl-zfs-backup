(in-package #:cl-zfs-backup.dataset)

(u:define-constant +prefix+ "clzb." :test #'string=)

(u:define-constant +prefix-end+ (length +prefix+) :test #'eql)

(defclass dataset ()
  ((%name
    :reader name
    :initarg :name)
   (%endpoint
    :reader endpoint
    :initarg :endpoint)
   (%holds
    :reader holds
    :initform (u:dict #'equalp))
   (%bytes-referenced
    :reader bytes-referenced
    :initarg :bytes-referenced)))

(u:define-printer (dataset stream :identity nil)
  (format stream "~a" (name dataset)))

(defgeneric exists? (dataset))

(defgeneric update (dataset data))

(defgeneric destroy (dataset))
