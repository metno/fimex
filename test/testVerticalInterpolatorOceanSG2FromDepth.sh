#! /bin/sh
echo "testing conversion of depth to ocean_s_g2"
TEST_SRCDIR=$(dirname $0)
OUT_NC="out$$.nc"
./fimex.sh \
    --input.file=${TEST_SRCDIR}/verticalOceanDepth.nc \
    --input.config "${TEST_SRCDIR}/verticalOceanSG2_tohybrid.ncml" \
    --verticalInterpolate.type=depth \
    --verticalInterpolate.ignoreValidityMin=true \
    --verticalInterpolate.method=linear \
    --verticalInterpolate.templateVar to_hybrid_template \
    --output.file="$OUT_NC"
if [ $? != 0 ]; then
  echo "failed converting depth to ocean_s_g2"
  rm -f "$OUT_NC"
  exit 1
fi
EXP_NC="${TEST_SRCDIR}/verticalOceanSG2_tohybrid.nc"
if ./cdmcmp "$EXP_NC" "$OUT_NC" ; then
  echo "success"
  E=0
else
  echo "failed diff ocean_s_g2 to depth"
  E=1
fi
rm -f "$OUT_NC"
exit $E
