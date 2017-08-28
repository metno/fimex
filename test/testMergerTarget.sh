#! /bin/sh
echo "test merge to target grid via fimex command-line"

TEST_SRCDIR=$(dirname $0)
BASE="${TEST_SRCDIR}/merge_target_base.nc"
TOP="${TEST_SRCDIR}/merge_target_top.nc"
VAR="air_temperature_2m"
VARBASE="sea_surface_temperature"

if [ ! -f "$BASE" -o ! -f "$TOP" ]; then
   echo "no base/top input: $BASE / $TOP, skipping test..."
   exit 0;
fi
./fimex.sh --input.file="$BASE" --merge.inner.file="$TOP" \
    --merge.method=bilinear \
    --merge.projString="+proj=stere +lat_0=90 +lon_0=70 +lat_ts=60 +units=m +a=6.371e+06 +e=0 +no_defs" \
    --merge.xAxisValues="-1192800,-1192000,...,-1112800" --merge.yAxisValues="-1304000,-1303200,...,-1224000" \
    --merge.xAxisUnit="m" --merge.yAxisUnit="m" \
    --output.file=out$$.nc
if [ $? != 0 ]; then
  echo "failed merge to target grid via fimex command-line"
  exit 1
fi

./fimex.sh --input.file out$$.nc --input.printNcML 2>/dev/null | grep -q "<variable \+name=\"$VAR\""
if [ $? != 0 ]; then
  echo "failed merge to target grid content via fimex command-line"
  exit 1
fi

# same, but this time keeping some outer variables
./fimex.sh --input.file="$BASE" --merge.inner.file="$TOP" \
    --merge.keepOuterVariables \
    --merge.method=bilinear \
    --merge.projString="+proj=stere +lat_0=90 +lon_0=70 +lat_ts=60 +units=m +a=6.371e+06 +e=0 +no_defs" \
    --merge.xAxisValues="-1192800,-1192000,...,-1112800" --merge.yAxisValues="-1304000,-1303200,...,-1224000" \
    --merge.xAxisUnit="m" --merge.yAxisUnit="m" \
    --extract.selectVariables=$VAR \
    --extract.selectVariables=$VARBASE \
    --output.file=out$$.nc
if [ $? != 0 ]; then
  echo "failed merge to target grid via fimex command-line keeping outer variables"
  exit 1
fi

./fimex.sh --input.file out$$.nc --input.printNcML 2>/dev/null | grep -q "<variable \+name=\"$VAR\""
if [ $? != 0 ]; then
  echo "failed merge to target grid content via fimex command-line keeping outer variables: $VAR not found"
  exit 1
fi
./fimex.sh --input.file out$$.nc --input.printNcML 2>/dev/null | grep -q "<variable \+name=\"$VARBASE\""
if [ $? != 0 ]; then
  echo "failed merge to target grid content via fimex command-line, keeping outer variable $VARBASE"
  exit 1
fi


echo "success"
rm -f out$$.nc
exit 0
