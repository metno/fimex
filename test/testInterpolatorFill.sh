#!/bin/sh

TEST_SRCDIR=`dirname $0`
TEST_BINDIR=`pwd`

cd "$TEST_SRCDIR"

INPUT_FILE="coordTest.nc"
OUTPUT_FILE="$TEST_BINDIR/coordTestFilled.nc"

if [ ! -f "$INPUT_FILE" ]; then
    echo "missing test data file '$INPUT_FILE'"
    exit 1;
fi

TESTCASE="interpolator with prefill"
echo "testing $TESTCASE"
"${TEST_BINDIR}/fimex.sh" \
    -c fillInterpolator.cfg \
    --input.file "$INPUT_FILE" \
    --output.file "$OUTPUT_FILE"
if [ $? != 0 ]; then
  echo "failed $TESTCASE"
  exit 1
fi
echo "success $TESTCASE"

TESTCASE="interpolator with postfill"
echo "testing $TESTCASE"
"${TEST_BINDIR}/fimex.sh" \
    --input.file "$INPUT_FILE" \
    --output.file "$OUTPUT_FILE" \
    --interpolate.method nearestneighbor \
    --interpolate.projString "+proj=stere +lat_0=90 +lon_0=0 +lat_ts=60 +units=m +a=6.371e+06 +e=0 +no_defs" \
    --interpolate.preprocess "fill2d(0.01, 1.6, 1000)" \
    --interpolate.xAxisValues "-1705516, -1655353, -1605191, -1555029, -1504867, -1454704, -1404542,-1354380, -1304218, -1254056, -1203893" \
    --interpolate.yAxisValues "-6872225, -6822063, -6771901, -6721738, -6671576, -6621414, -6571252,-6521089, -6470927, -6420765, -6370603" \
    --interpolate.xAxisUnit m \
    --interpolate.yAxisUnit m
if [ $? != 0 ]; then
  echo "failed $TESTCASE"
  exit 1
fi
echo "success $TESTCASE"

exit 0
