#! /bin/sh
TEST_SRCDIR=$(dirname $0)
TEST="printSize"

echo "testing $TEST"
./fimex.sh --input.file "${TEST_SRCDIR}/printsize.nc" --input.printSize
if [ $? != 0 ]; then
  echo "failed $TEST"
  exit 1
fi
echo "success"
exit 0
