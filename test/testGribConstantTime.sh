#! /bin/sh
set -e

TEST_SRCDIR=$(dirname $0)
TESTCASE="grib reader constant time"

echo "testing $TESTCASE"

TEST_GCT="${TEST_SRCDIR}/grib/constantTime"

if ./fimex.sh \
       --input.file "$TEST_GCT/input.grib2" \
       --input.type grib \
       --input.config "$TEST_GCT/config.xml" \
       --output.file "$TEST_GCT/output.nc" \
       --output.type nc4 ; then
    echo "success"
    E=0
else
    echo "failed $TESTCASE"
    E=1
fi
rm -vf "$TEST_GCT/output.nc"
exit $E
