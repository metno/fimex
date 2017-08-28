#! /bin/sh

TEST_SRCDIR=$(dirname $0)

INPUTFILE="${TEST_SRCDIR}/char.nc"
OUTPUTFILE="out$$.nc"
TESTCASE="writing netcdf byte and char"

echo "test ${TESTCASE}"
if [ ! -f "${INPUTFILE}" ]; then
   echo "no input data: ${INPUTFILE}, skipping test..."
   exit 0;
fi
./fimex.sh --input.file "${INPUTFILE}" --output.file "${OUTPUTFILE}"
if [ $? != 0 ]; then
  echo "failed ${TESTCASE}"
  exit 1
fi
echo "success"
rm -f "${OUTPUTFILE}"
exit 0
