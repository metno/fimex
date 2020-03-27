#! /bin/sh
echo "test reading with ncml"

TEST_SRCDIR=`dirname $0`
TEST_BINDIR=`pwd`
cd "$TEST_SRCDIR"

if [ ! -f coordTest.nc ]; then
   echo "FAIL missing 'coordTest.nc'"
   exit 1
fi
if [ ! -f test.ncml ]; then
   echo "FAIL missing 'test.ncml'"
   exit 1
fi

"${TEST_BINDIR}/fimex.sh" --input.file=test.ncml --output.file=x --output.type=null
if [ $? != 0 ]; then
  echo "failed reading ncml"
  exit 1
fi

"${TEST_BINDIR}/fimex.sh" --input.file=coordTest.nc --input.config=test.ncml --output.file=x --output.type=null
if [ $? != 0 ]; then
  echo "failed reading nc with ncml-config"
  exit 1
fi

"${TEST_BINDIR}/fimex.sh" --input.file="glob:data/joinExistingAgg*.nc" --output.file=x --output.type=null
if [ $? != 0 ]; then
  echo "failed reading glob:data/joinExistingAgg*.nc"
  exit 1
fi

echo "success"
exit 0
