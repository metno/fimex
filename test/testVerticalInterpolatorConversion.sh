#! /bin/sh
TEST_SRCDIR=$(dirname $0)
TEST="conversion of humidity, addition of pressure field"
echo "testing $TEST"
OUT_NC="out$$.nc"
./fimex.sh \
    --input.file=${TEST_SRCDIR}/testdata_conversion_in.nc \
    --output.type nc4 \
    --verticalInterpolate.dataConversion=specific2relative \
    --verticalInterpolate.dataConversion=omega2vwind \
    "--verticalInterpolate.dataConversion=add4Dpressure;air_pressure4D=press4d" \
    --output.file="$OUT_NC"
if [ $? != 0 ]; then
  echo "failed $TEST"
  rm -f "$OUT_NC"
  exit 1
fi

EXP_NC=${TEST_SRCDIR}/testdata_conversion_ex.nc
if ./nccmp.sh "$EXP_NC" "$OUT_NC" ; then
  echo "success"
  E=0
else
  echo "failed diff $TEST"
  E=1
fi
rm -f "$OUT_NC"
exit $E
