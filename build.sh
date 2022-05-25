#!/bin/sh

if [ -z "$2" ]; then
  echo "usage: $0 out-file compression-level"
  exit 1
else
  sbcl \
    --noinform \
    --disable-ldb \
    --disable-debugger \
    --eval "(asdf:load-system :cl-zfs-backup)" \
    --eval "(uiop:symbol-call \"CL-ZFS-BACKUP\" \"BUILD\" \"$1\" $2)"
fi
