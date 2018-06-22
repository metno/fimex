#! /bin/sh

TEST_SRCDIR=$(dirname $0)
TESTCASE="interpolating file with NC_STRINGs"

echo "testing $TESTCASE"

OUT_NC="out$$.nc"
./fimex.sh \
   --input.file ${TEST_SRCDIR}/ncStringInput.nc \
   --interpolate.method bilinear \
   --interpolate.longitudeValues  4.15 \
   --interpolate.latitudeValues  58.15 \
   --output.file "$OUT_NC" --output.config ${TEST_SRCDIR}/ncStringWriter.xml
if [ $? != 0 ]; then
  echo "failed $TESTCASE"
  rm -f "$OUT_NC"
  exit 1
fi

EXP_NC=${TEST_SRCDIR}/ncStringExpected.nc
if ./nccmp.sh "$EXP_NC" "$OUT_NC" ; then
  echo "success"
  E=0
else
  echo "failed diff $TESTCASE"
  E=1
fi
rm -f "$OUT_NC"
exit $E
