#! /bin/sh
echo "test quality extractor / masking via fimex command-line"

TEST_SRCDIR=$(dirname $0)
DATA="${TEST_SRCDIR}/testQEmask_data.nc"
MASK="${TEST_SRCDIR}/testQEmask_mask.nc"
# conf file from local directory: that file is also auto-generated
CONF="testQEmask.xml"

if [ ! -f "$DATA" -o ! -f "$MASK" -o ! -f "$CONF" ]; then
   echo "no input data/mask/config: $DATA / $MASK / $CONF, skipping test..."
   exit 0;
fi
./fimex.sh --input.file="$DATA" --qualityExtract.config="$CONF" --output.file=out$$.nc
if [ $? != 0 ]; then
  echo "failed masking via fimex command-line"
  exit 1
fi
echo "success"
rm -f out$$.nc
exit 0
