#! /bin/sh
echo "testing conversion of sigma-hybrid to pressure"
TEST_SRCDIR=$(dirname $0)
OUT_NC="out$$.nc"
./fimex.sh \
    --input.file=${TEST_SRCDIR}/verticalSigmaHybrid.nc \
    --verticalInterpolate.type=pressure \
    --verticalInterpolate.method=log \
    --verticalInterpolate.level1=1000,850,500,300,100,50 \
    --output.file="$OUT_NC"
if [ $? != 0 ]; then
  echo "failed converting sigma-hybrid to pressure"
  rm -f "$OUT_NC"
  exit 1
fi

EXP_NC="${TEST_SRCDIR}/verticalSigmaHybridPressure.nc"
if "${TEST_SRCDIR}/nccmp.sh" "$EXP_NC" "$OUT_NC" ; then
  echo "success"
  E=0
else
  echo "failed diff sigma-hybrid to pressure"
  E=1
fi
rm -f "$OUT_NC"
exit $E
