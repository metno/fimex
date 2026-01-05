#! /bin/sh
TEST="interpolation from sigma-hybrid to reduced sigma-hybrid"
echo "testing $TEST"
TEST_SRCDIR=$(dirname $0)
OUT_NC="out$$.nc"

# verticalSigmaHybridReduced.nc contains values produced by only
# keeping k-level 0,3,...,57 by using extract.pickDimension

# this test should reproduce the values by vertical interpolation

./fimex.sh \
    --input.file "${TEST_SRCDIR}/verticalSigmaHybrid.nc" \
    --input.config "${TEST_SRCDIR}/verticalSigmaHybrid_reduced.ncml" \
    --verticalInterpolate.type=pressure \
    --verticalInterpolate.method=linear \
    --verticalInterpolate.templateVar "to_hybrid_template" \
    --output.file="$OUT_NC"

if [ $? != 0 ]; then
  echo "failed $TEST"
  rm -f "$OUT_NC"
  exit 1
fi

EXP_NC="${TEST_SRCDIR}/verticalSigmaHybridReduced.nc"
if ./cdmcmp "$EXP_NC" "$OUT_NC" ; then
  echo "success"
  E=0
else
  echo "failed diff for $TEST"
  E=1
fi
rm -f "$OUT_NC"
exit $E
