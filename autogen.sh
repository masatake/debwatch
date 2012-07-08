#!/bin/sh

chmod u+x $0

set -x

LANG=C
rm -rf autom4te.cache

aclocal -I misc/m4
automake --add-missing --force-missing -Wno-portability
autoconf

