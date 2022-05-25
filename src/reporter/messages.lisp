(in-package #:cl-zfs-backup.reporter)

(defgeneric message (level category type))

(defmacro define-message ((level category type) &body format-string)
  `(defmethod message ((level (eql ,level)) (category (eql ,category)) (type (eql ,type)))
     ,@format-string))

(define-message (:error :config :invalid-name)
  "~(~a~): name must be a symbol")

(define-message (:error :config :invalid-plist)
  "~(~a~): options must be a property list")

(define-message (:error :config :invalid-plist/named)
  "~(~a~) '~(~a~)': options must be a property list")

(define-message (:error :config :invalid-keys)
  "~(~a~): invalid options: ~{~s~^, ~}")

(define-message (:error :config :invalid-keys/named)
  "~(~a~) '~(~a~)': invalid options: ~{~s~^, ~}")

(define-message (:error :config :duplicate-keys)
  "~(~a~): duplicate options specified: ~{~s~^, ~}")

(define-message (:error :config :duplicate-keys/named)
  "~(~a~) '~(~a~)': duplicate options specified: ~{~s~^, ~}")

(define-message (:error :config :missing-key)
  "~(~a~): option ~s must be specified")

(define-message (:error :config :missing-key/named)
  "~(~a~) '~(~a~)': option ~s must be specified")

(define-message (:error :config :invalid-string)
  "~(~a~): option ~s must be a string")

(define-message (:error :config :invalid-string/named)
  "~(~a~) '~(~a~)': option ~s must be a string")

(define-message (:error :config :invalid-boolean)
  "~(~a~): ~s must be T or NIL")

(define-message (:error :config :invalid-boolean/named)
  "~(~a~) '~(~a~)': ~s must be T or NIL")

(define-message (:error :config :policy-invalid-value)
  "policy '~(~a~)': option ~s must be a positive integer")

(define-message (:error :config :source-invalid-exclude-use)
  "source '~(~a~)': option :EXCLUDE is not valid without :RECURSIVE T")

(define-message (:error :config :source-invalid-exclude)
  "source '~(~a~)': option :EXCLUDE must be a list of strings")

(define-message (:error :config :source-invalid-exclude-path)
  "source '~(~a~)': option :EXCLUDE paths must not have a leading or trailing '/' character")

(define-message (:error :config :source-invalid-minimum-size)
  "source '~(~a~)': option :MINIMUM-SIZE must be a list of 2 elements; a non-negative integer and ~
any of: ~{~s~^, ~}")

(define-message (:error :config :source-undefined-policy)
  "source '~(~a~)': policy '~(~a~)' is not defined")

(define-message (:error :config :source-invalid-targets)
  "source '~(~a~)': option :TARGETS must be a list")

(define-message (:error :config :source-invalid-target)
  "source '~(~a~)': target form ~s: must be of the form (target-name :policy policy-name)")

(define-message (:error :config :source-undefined-target)
  "source '~(~a~)': target '~(~a~)' is not defined")

(define-message (:error :config :source-undefined-target-policy)
  "source '~(~a~)': target '~(~a~)' policy '~(~a~)' is not defined")

(define-message (:error :config :target-invalid-mode)
  "target '~(~a~)': option :MODE must be one of ~{~s~^, ~}")

(define-message (:error :config :target-invalid-base-path)
  "target '~(~a~)': option :BASE-PATH must not have a leading or trailing '/' character")

(define-message (:error :config :target-invalid-port)
  "target '~(~a~)': option :PORT must be an integer in the range of 1-65535")

(define-message (:error :config :process-invalid-interval)
  "process: option :INTERVAL must be a positive integer denoting minutes")

(define-message (:error :config :process-invalid-sources)
  "process: option :SOURCES must be a list of symbols")

(define-message (:error :config :process-undefined-source)
  "process: source '~(~a~)' is not defined")

(define-message (:error :config :process-invalid-log-level)
  "process: option ~s must be one of ~{~s~^, ~}")

(define-message (:error :config :file-not-found)
  "none of the configuration files ~{'~a'~^, ~} exist or are readable")

(define-message (:trace :command :trace)
  "executing command: `~a`")

(define-message (:error :command :failed)
  "executing command: `~a` failed with status: ~d")

(define-message (:info :process :file-logging)
  "logging to file: '~a'")

(define-message (:fatal :process :fatal-error)
  "fatal error:~%~a")

(define-message (:warn :process :user-abort-begin)
  "user aborted: waiting for jobs to finish...")

(define-message (:info :process :user-abort-end)
  "user aborted: exited gracefully")

(define-message (:info :process :cycle-begin)
  "started cycle")

(define-message (:info :process :cycle-end)
  "finished cycle~%")

(define-message (:info :collect :phase-begin)
  "started phase 1/4: collect")

(define-message (:info :collect :phase-end)
  "finished phase 1/4: collect")

(define-message (:error :collect :permission-parse-failed)
  "unexpected token ~s for node ~a while parsing permissions")

(define-message (:warn :collect :source-missing-permissions)
  "source '~(~a~)': skipping: missing required permissions on filesystem '~a': ~{~a~^ ~}")

(define-message (:warn :collect :target-missing-permissions)
  "source '~(~a~)': skipping target '~(~a~)' on host '~(~a~)': missing required permissions on ~
filesystem '~a': ~{~a~^ ~}")

(define-message (:warn :collect :source-missing-base-path)
  "source '~(~a~)': skipping: missing base-path filesystem '~a'")

(define-message (:warn :collect :target-missing-base-path)
  "source '~(~a~)': skipping target '~(~a~)' on host '~(~a~)': missing base-path filesystem '~a'")

(define-message (:warn :collect :target-unconnectable)
  "source '~(~a~)': skipping target '~(~a~)': failed to connect to host '~(~a~)")

(define-message (:notice :collect :target-reconnected)
  "source '~(~a~)': target '~(~a~)' is back online")

(define-message (:debug :collect :source-begin)
  "collecting datasets for source: '~(~a~)'")

(define-message (:debug :collect :source-end)
  "finished collecting datasets for source: '~(~a~)'")

(define-message (:debug :collect :target-begin)
  "collecting datasets for target: '~(~a~)'")

(define-message (:debug :collect :target-end)
  "finished collecting datasets for target: '~(~a~)'")

(define-message (:info :snapshot :phase-begin)
  "started phase 2/4: snapshot")

(define-message (:info :snapshot :phase-end)
  "finished phase 2/4: snapshot")

(define-message (:debug :snapshot :create-attempt)
  "attempting to create snapshot of filesystem: '~a'")

(define-message (:debug :snapshot :below-minimum-size)
  "skipping filesystem '~a': changed size ~a is below minimum size of ~a")

(define-message (:info :snapshot :create-complete)
  "created snapshot: '~a'")

(define-message (:info :replicate :phase-begin)
  "started phase 3/4: replicate")

(define-message (:info :replicate :phase-end)
  "finished phase 3/4: replicate")

(define-message (:debug :replicate :attempt)
  "attempting ~(~a~)~@[ ~(~a~)~] replication of filesystem '~a' to host '~a'")

(define-message (:debug :replicate :filesystem-created)
  "filesystem '~a' created on host '~(~a~)'")

(define-message (:warn :replicate :destructive-failed)
  "skipping replication of filesystem '~a' to target '~(~a~)': destructive replication is not ~
possible; please set `:DESTRUCTIVE T` for this target")

(define-message (:warn :replicate :incremental-failed)
  "non-destructive incremental replication of filesystem '~a' to target ~(~a~) is not possible. ~
Trying a destructive incremental replication")

(define-message (:debug :replicate :incremental-skipped)
  "incremental replication skipped for filesystem '~a': common snapshot is the same as the most ~
recent snapshot")

(define-message (:debug :replicate :hold)
  "hold tag '~a' added to snapshot '~a' on host '~a'")

(define-message (:debug :replicate :unhold)
  "hold tag '~a' removed from snapshot '~a' on host '~a'")

(define-message (:info :replicate :begin)
  "replicating filesystem '~a' to host '~a'")

(define-message (:info :replicate :end)
  "finished ~(~a~)~@[ ~(~a~)~] replication of filesystem '~a' to host '~a'")

(define-message (:warn :replicate :cleanup-begin)
  "cleaning up due to replication failure")

(define-message (:warn :replicate :cleanup-end)
  "finished cleaning up due to replication failure")

(define-message (:info :prune :phase-begin)
  "started phase 4/4: prune")

(define-message (:info :prune :phase-end)
  "finished phase 4/4: prune")

(define-message (:info :prune :filesystem)
  "pruned ~:d snapshot~:p from filesystem '~a' on host '~(~a~)'")

(define-message (:info :prune :source)
  "pruned a total of ~:d snapshot~:p, reclaiming ~a, for source '~a'")

(define-message (:info :prune :target)
  "pruned a total of ~:d snapshot~:p, reclaiming ~a, for source '~(~a~)' on host '~(~a~)'")

(define-message (:error :process :dependencies-not-found)
  "the following external application dependencies were not satisfied:~%~{~a~^~%~}")
