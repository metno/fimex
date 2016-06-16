#!/bin/sh

set -e

NC1="$1"
NC2="$2"

if test ! -r "$NC1" ; then
    echo "No file '$NC1'"
    exit 1
fi
if test ! -r "$NC2" ; then
    echo "No file '$NC2'"
    exit 1
fi

if cmp --quiet "$NC1" "$NC2"; then
    exit 0
fi

NCDUMP=ncdump # debian: netcdf-bin
MD5SUM=md5sum # debian: package coreutils

if ! $NCDUMP 2> /dev/null; then
    echo "No ncdump, cannot compare file contents of '$NC1' and '$NC2'"
    exit 1
fi
if ! $MD5SUM --help > /dev/null; then
    echo "No md5sum, cannot compare file contents of '$NC1' and '$NC2'"
    exit 1
fi

nc5sum() {
    $NCDUMP -n anonymous -p 8,15 "$1" | $MD5SUM | cut -d' ' -f1
}

NC1MD5=`nc5sum "$NC1"`
NC2MD5=`nc5sum "$NC2"`

test "$NC1MD5" = "$NC2MD5"
