# cl-zfs-backup

## Overview

A Common Lisp application to automate the creation of snapshots, replicate them incrementally to one
or more hosts, and manage their retention on a per source, per destination, per filesystem basis.

## Install

Binaries are included for x86_64 for both Linux and FreeBSD. They can be found on the releases page.

For building from source, Steel Bank Common Lisp must be installed. There exists a convenience
build.sh script in the root of the project for automating the build process.

## TODO

Documentation, and more documentation. For now, see the included clzb.conf file in the project root.

## License

Copyright © 2022 Michael Fiano <mail@mfiano.net>.

Licensed under the MIT License.
