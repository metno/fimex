#! /bin/sh
TESTNAME="ncml aggregation with config files"
echo "testing $TESTNAME"

TEST_SRCDIR=`dirname $0`
TEST_BINDIR=`pwd`
cd "$TEST_SRCDIR"

file="${TEST_BINDIR}/ncmlagg_$$.nc"
"${TEST_BINDIR}/fimex.sh" test_ncmlagg_with_config_merge.ncml --output.file "${file}"
if [ $? != 0 ]; then
  echo "failed $TESTNAME"
  rm -f "${TEST_BINDIR}/${file}"
  exit 1
fi

if "${TEST_BINDIR}/nccmp.sh" "test_ncmlagg_with_config_expected.nc" "${file}" ; then
    echo "success $TESTNAME"
    E=0
else
    echo "failed $TESTNAME";
    E=1
fi
rm -f "${file}"
exit $E
