#! /bin/sh
TEST="interpolation height to altitude"
echo "testing $TEST"
TEST_SRCDIR=$(dirname $0)
OUT_NC="out$$.nc"
./fimex.sh \
    --verticalInterpolate.type=altitude \
    --verticalInterpolate.level1=800,400,100,50 \
    --verticalInterpolate.method=linear_const_extra \
    --input.file=${TEST_SRCDIR}/testdata_altitude_height_in.nc \
    --output.file="$OUT_NC" --output.type=nc4
if [ $? != 0 ]; then
  echo "failed $TEST"
  rm -f "$OUT_NC"
  exit 1
fi

EXP_NC="${TEST_SRCDIR}/testdata_altitude_height_ex.nc"
if ./nccmp.sh "$EXP_NC" "$OUT_NC" ; then
  echo "success"
  E=0
else
  echo "failed diff for $TEST"
  E=1
fi
rm -f "$OUT_NC"
exit $E
