#! /bin/sh

TEST="interpolation with 2d lonlat grid mapping"
echo "testing $TEST"

TEST_NC="${TEST_EXTRADATA_DIR}/grid_mapping_lonlat2d.nc"
TEST_OC="${TEST_EXTRADATA_DIR}/output-nc4.xml"
if [ ! -f "${TEST_NC}" -o ! -f "${TEST_OC}" ]; then
   echo "SKIP missing optional test data '${TEST_NC}' and/or config '${TEST_OC}'"
   exit 0
fi

OUT="grid_mapping_lonlat2d_interpolated.nc"

./fimex.sh \
    --input.file "$TEST_NC" \
    --interpolate.projString "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" \
    --interpolate.xAxisUnit m \
    --interpolate.yAxisUnit m \
    --interpolate.xAxisValues "520000,525000,...,535000" \
    --interpolate.yAxisValues "7440000,7441000,...,7448000" \
    --interpolate.method forward_mean \
    --output.file "$OUT" \
    --output.type nc4 \
    --output.config "$TEST_OC"

E=$?
if [ $E != 0 ]; then
  echo "failed $TEST"
else
    echo "success"
fi
rm -f "$OUT"
exit $E
