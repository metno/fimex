#! /bin/sh
if [ ! -f test.nc ]; then
	exit 0;
fi
echo "testing conversion NetCDF to NetCDF"
../src/binSrc/utplukk test.nc testNcRdWr.nc
if [ $? != 0 ]; then
  echo "failed converting nc to nc"
  rm -f testNcRdWr.nc
  rm -f test.nc
  exit 1
fi
diff test.nc testNcRdWr.nc 
if [ $? != 0 ]; then
  echo "failed diff nc to nc"
  rm -f testNcRdWr.nc
  rm -f test.nc
  exit 1
fi
rm -f testNcRdWr.nc
rm -f test.nc
echo "success"
exit 0
