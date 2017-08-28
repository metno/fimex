#! /bin/sh
echo "test merge via fimex command-line"

TEST_SRCDIR=$(dirname $0)
INNER="${TEST_SRCDIR}/test_merge_inner.nc"
OUTER="${TEST_SRCDIR}/test_merge_outer.nc"
VAR="ga_2t_1"

if [ ! -f "$INNER" -o ! -f "$OUTER" ]; then
   echo "no inner/outer input: $INNER / $OUTER, skipping test..."
   exit 0;
fi
./fimex.sh --input.file="$OUTER" --merge.inner.file="$INNER" \
    --merge.method=bilinear \
    --output.file=out$$.nc
if [ $? != 0 ]; then
  echo "failed merge via fimex command-line"
  exit 1
fi

./fimex.sh --input.file out$$.nc --input.printNcML 2>/dev/null | grep -q "<variable \+name=\"$VAR\""
if [ $? != 0 ]; then
  echo "failed merge content via fimex command-line"
  exit 1
fi

echo "success"
rm -f out$$.nc
exit 0
