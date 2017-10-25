#! /bin/sh
srcdir=`dirname $0`
echo "testing conversion NetCDF to NetCDF"

if test -n "$TEST_EXTRADATA_DIR"; then
    TEST_EXTRADATADIR="${TEST_SRCDIR}"
fi

TEST_DAT="${TEST_EXTRADATA_DIR}/flth00.dat"
if [ ! -f "${TEST_DAT}" ]; then
   echo "no input data: '${TEST_DAT}', skipping test..."
   exit 0
fi

FILE1=test_feltNetcdfWrite.nc
if [ ! -f "${FILE1}" ]; then
   echo "missing input data: '${FILE1}'"
   exit 1
fi

./fimex.sh "${FILE1}" testNcRdWr.nc

if [ $? != 0 ]; then
  echo "failed converting nc to nc"
  rm -f testNcRdWr.nc "${FILE1}"
  exit 1
fi

if "${srcdir}/nccmp.sh" "${FILE1}" testNcRdWr.nc ; then
  echo "success"
  E=0
else
  echo "failed diff nc to nc"
  E=1
fi
rm -f testNcRdWr.nc "${FILE1}"
exit $E
