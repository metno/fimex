#! /bin/sh

set -e

TEST="printNcML with NAT"
echo "testing $TEST"

TEST_SRCDIR=`dirname $0`
TEST_BINDIR=`pwd`
cd "${TEST_SRCDIR}"

if test -n "$TEST_EXTRADATA_DIR"; then
    TEST_EXTRADATADIR="${TEST_SRCDIR}"
fi

TEST_DAT="${TEST_EXTRADATA_DIR}/flth00.dat"
if [ ! -f "${TEST_DAT}" ]; then
   echo "SKIP $TEST, missing optional '${TEST_DAT}'"
   exit 0
fi

file="${TEST_BINDIR}/test_printncml_$$.ncml"

"${TEST_BINDIR}/fimex.sh" \
    -c felt2netcdf.cfg \
    --input.file "${TEST_DAT}" \
    --input.printNcML="${file}"

E=1
if [ $? != 0 ]; then
    echo "FAIL $TEST, exit code != 0"
elif [ ! -f" ${file}" ]; then
    echo "FAIL $TEST, missing ncml output ${file}"
elif test "`head -n1 $file`" != '<?xml version="1.0" encoding="UTF-8"?>' ; then
    echo "FAIL $TEST, ncml output does not seem to be xml"
elif grep -q '<!-- datatype NAT translated to INT for ncml -->' "${file}" ; then
    echo "PASS $TEST"
    E=0
else
    echo "FAIL $TEST (missing NAT -> INT comment)";
fi
rm -f "${file}"
exit $E
