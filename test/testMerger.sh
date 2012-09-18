#! /bin/sh
# test/testMerger.sh.  Generated from testMerger.sh.in by configure.
echo "test merge via fimex command-line"

INNER="../test/test_merge_inner.nc"
OUTER="../test/test_merge_outer.nc"
VAR="ga_2t_1"

if [ ! -f "$INNER" -o ! -f "$OUTER" ]; then
   echo "no inner/outer input: $INNER / $OUTER, skipping test..."
   exit 0;
fi
../src/binSrc/fimex --input.file="$OUTER" --merge.inner.file="$INNER" \
    --merge.method.outer=bilinear \
    --merge.variables.inner "$VAR" --merge.variables.outer "$VAR" \
    --output.file=out.nc
if [ $? != 0 ]; then
  echo "failed merge via fimex command-line"
  exit 1
fi

../src/binSrc/fimex --input.file out.nc --input.printNcML 2>/dev/null | grep -q "<variable \+name=\"$VAR\""
if [ $? != 0 ]; then
  echo "failed merge content via fimex command-line"
  exit 1
fi

echo "success"
exit 0
