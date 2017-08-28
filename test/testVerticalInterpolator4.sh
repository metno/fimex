#! /bin/sh
TEST="conversion of ensemble sigma-hybrid to pressure"
TEST_SRCDIR=$(dirname $0)
echo "testing $TEST"
OUT_NC="out$$.nc"
./fimex.sh \
    --input.file=${TEST_SRCDIR}/testdata_vertical_ensemble_in.nc \
    --verticalInterpolate.type=pressure \
    --verticalInterpolate.method=log \
    --verticalInterpolate.level1=1000,850,500,300,100,50 \
    --output.file="$OUT_NC"
if [ $? != 0 ]; then
  echo "failed $TEST"
  rm -f "$OUT_NC"
  exit 1
fi

EXP_NC="${TEST_SRCDIR}/testdata_vertical_ensemble_ex.nc"
if "${TEST_SRCDIR}/nccmp.sh" "$EXP_NC" "$OUT_NC" ; then
  echo "success"
  E=0
else
  echo "failed diff for $TEST"
  E=1
fi
rm -f "$OUT_NC"
exit $E
