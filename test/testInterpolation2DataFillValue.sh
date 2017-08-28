#! /bin/sh

TESTCASE="fillvalue handling in interpolation array to data conversion"
echo "test $TESTCASE (fimex command-line)"

TEST_SRCDIR=$(dirname $0)
IN_NC="${TEST_SRCDIR}/testdata_int2data_in.nc"
EXP_NC="${TEST_SRCDIR}/testdata_int2data_ex.nc"
OUT_NC="out$$.nc"

./fimex.sh \
    --input.file="$IN_NC" \
    --interpolate.method=bilinear \
    --interpolate.latitudeValues=61 \
    --interpolate.longitudeValues=11 \
    --interpolate.preprocess="creepfill2d(5,2)" \
    --output.file="$OUT_NC" --output.type=nc4
if [ $? != 0 ]; then
  echo "failed $TESTCASE"
  rm -f "$OUT_NC"
  exit 1
fi
if "${TEST_SRCDIR}/nccmp.sh" "$EXP_NC" "$OUT_NC" ; then
  echo "success"
  E=0
else
  echo "failed diff $TESTCASE"
  E=1
fi
rm -f "$OUT_NC"
exit $E
