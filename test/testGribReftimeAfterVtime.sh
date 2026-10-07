#! /bin/sh
TEST="grib write with reftime after validtime"
echo "test $TEST"

TEST_BINDIR=`pwd`
TEST_SRCDIR="$(dirname $0)"

# grib writer config refers to template file by relative path
cd "${TEST_SRCDIR}/grib/reftimeAfterFirstTime"

OUT="testGribReftimeAfterVtime_$$.grb"
"$TEST_BINDIR/fimex.sh" \
    --input.file "snapXmini.nc" \
    --extract.selectVariables Cs137_concentration \
    --output.file "$OUT" \
    --output.type grib1 \
    --output.config "cdmGribWriterConfig.xml"

if [ $? != 0 ]; then
  echo "failed $TEST"
  exit 1
fi

echo "success $TEST"
rm -f "$OUT"
exit 0
