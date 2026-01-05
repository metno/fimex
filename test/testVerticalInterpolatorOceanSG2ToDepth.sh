#! /bin/sh
echo "testing conversion of ocean_s_g2 to depth"
TEST_SRCDIR=$(dirname $0)
OUT_NC="out$$.nc"
./fimex.sh \
    --input.file=${TEST_SRCDIR}/verticalOceanSG2.nc \
    --verticalInterpolate.type=depth \
    --verticalInterpolate.ignoreValidityMin=true \
    --verticalInterpolate.method=linear \
    --verticalInterpolate.level1=0,3,10,15,25,50,75,100,150,200,250,300,500,700,1000,2000,3000 \
    --output.file="$OUT_NC"
if [ $? != 0 ]; then
  echo "failed converting ocean_s_g2 to depth"
  rm -f "$OUT_NC"
  exit 1
fi
EXP_NC="${TEST_SRCDIR}/verticalOceanSG2_depth.nc"
if ./cdmcmp "$EXP_NC" "$OUT_NC" ; then
  echo "success"
  E=0
else
  echo "failed diff ocean_s_g2 to depth"
  E=1
fi
rm -f "$OUT_NC"
exit $E
