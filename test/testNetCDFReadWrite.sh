#! /bin/sh
if [ ! -f test.nc ]; then
	exit 0;
fi
srcdir=`dirname $0`
echo "testing conversion NetCDF to NetCDF"
if [ ! -f "${srcdir}/flth00.dat" ]; then
   echo "no input data: flth00.dat, skipping test..."
   exit 0;
fi
../src/binSrc/fimex test.nc testNcRdWr.nc
if [ $? != 0 ]; then
  echo "failed converting nc to nc"
  rm -f testNcRdWr.nc
  rm -f test.nc
  exit 1
fi

if "${srcdir}/nccmp.sh" test.nc testNcRdWr.nc ; then
  echo "success"
  E=0
else
  echo "failed diff nc to nc"
  E=1
fi
rm -f testNcRdWr.nc
rm -f test.nc
exit $E
