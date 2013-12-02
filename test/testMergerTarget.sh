#! /bin/sh
# test/testMergerTarget.sh.  Generated from testMergerTarget.sh.in by configure.
echo "test merge to target grid via fimex command-line"

BASE="../test/merge_target_base.nc"
TOP="../test/merge_target_top.nc"
VAR="air_temperature_2m"

if [ ! -f "$BASE" -o ! -f "$TOP" ]; then
   echo "no base/top input: $BASE / $TOP, skipping test..."
   exit 0;
fi
../src/binSrc/fimex --input.file="$BASE" --merge.inner.file="$TOP" \
    --merge.method=bilinear \
    --merge.projString="+proj=stere +lat_0=90 +lon_0=70 +lat_ts=60 +units=m +a=6.371e+06 +e=0 +no_defs" \
    --merge.xAxisValues="-1192800,-1192000,...,-1112800" --merge.yAxisValues="-1304000,-1303200,...,-1224000" \
    --merge.xAxisUnit="m" --merge.yAxisUnit="m" \
    --output.file=out.nc
if [ $? != 0 ]; then
  echo "failed merge to target grid via fimex command-line"
  exit 1
fi

../src/binSrc/fimex --input.file out.nc --input.printNcML 2>/dev/null | grep -q "<variable \+name=\"$VAR\""
if [ $? != 0 ]; then
  echo "failed merge to target grid content via fimex command-line"
  exit 1
fi

echo "success"
exit 0
