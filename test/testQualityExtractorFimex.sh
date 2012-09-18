#! /bin/sh
# test/testQualityExtractorFimex.sh.  Generated from testQualityExtractorFimex.sh.in by configure.
echo "test quality extractor / masking via fimex command-line"

DATA="../test/testQEmask_data.nc"
MASK="../test/testQEmask_mask.nc"
CONF="../test/testQEmask.xml"

if [ ! -f "$DATA" -o ! -f "$MASK" -o ! -f "$CONF" ]; then
   echo "no input data/mask/config: $DATA / $MASK / $CONF, skipping test..."
   exit 0;
fi
../src/binSrc/fimex --input.file="$DATA" --qualityExtract.config="$CONF" --output.file=out.nc
if [ $? != 0 ]; then
  echo "failed masking via fimex command-line"
  exit 1
fi
echo "success"
exit 0
