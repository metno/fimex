#! /bin/sh

TEST="interpolation with 1d lon lat axes"
echo "testing $TEST"

TEST_NC="${TEST_EXTRADATA_DIR}/eez_ospar_areaCC_200185.nc"
if test ! -f "${TEST_NC}" ; then
   echo "missing input data '${TEST_NC}', skipping test..."
   exit 0
fi

OUT="eez_ospar_emep01.nc"

./fimex.sh \
    --input.file="$TEST_NC" \
    --interpolate.projString="+ellps=sphere +R=6370000 +proj=latlon +no_defs" \
    --interpolate.method=forward_sum \
    --interpolate.xAxisUnit=degrees_east \
    --interpolate.yAxisUnit=degrees_north \
    --interpolate.xAxisValues="-29.95,-29.85,...,89.95" \
    --interpolate.yAxisValues="30.05,30.15,...,81.95" \
    --output.file "$OUT" \
    --output.type=nc4

E=$?
if [ $E != 0 ]; then
  echo "failed $TEST"
else
    echo "success"
fi
rm -f "$OUT"
exit $E
